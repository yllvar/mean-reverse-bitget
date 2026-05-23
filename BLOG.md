# Building a Mean Reversion Trading Bot in OCaml

*A step-by-step guide for aspiring quantitative developers*

---

## Table of Contents

1. [Why OCaml for Trading?](#1-why-ocaml-for-trading)
2. [What is Mean Reversion?](#2-what-is-mean-reversion)
3. [Project Setup & Tooling](#3-project-setup--tooling)
4. [Exchange Authentication (HMAC-SHA256)](#4-exchange-authentication-hmac-sha256)
5. [Fetching Market Data](#5-fetching-market-data)
6. [The Strategy Logic](#6-the-strategy-logic)
7. [The Event Loop](#7-the-event-loop)
8. [Security & Key Management](#8-security--key-management)
9. [Putting It All Together](#9-putting-it-all-together)
10. [Next Steps](#10-next-steps)

---

## 1. Why OCaml for Trading?

Most algorithmic traders default to Python. Python is slow, dynamically typed, and prone to runtime errors that lose money. OCaml offers:

- **Type safety** — a runtime type error is impossible. If it compiles, it's correct.
- **Performance** — OCaml compiles to native code (LLVM or assembly). 10-100x faster than Python for numeric work.
- **Algebraic data types** — ideal for modelling trading signals, order states, and API responses.
- **Pattern matching** — write clear, exhaustive logic for every case.
- **Lwt** — lightweight cooperative threads for async I/O without the GIL or callback hell.

If you can write a trading bot in OCaml, you can write one in any language — but the reverse is not true.

## 2. What is Mean Reversion?

Mean reversion is the financial hypothesis that asset prices tend to return to their historical average over time. When a price deviates far from its mean, it creates a statistical opportunity to bet on the correction.

**The math:**

```
z = (current_price − μ) / σ
```

Where:
- μ = mean of the last N prices
- σ = standard deviation of those prices
- z = number of standard deviations from the mean

**Trading rules:**
- `z < −1.5` → price is abnormally low → **BUY** (expect reversion upward)
- `z > +1.5` → price is abnormally high → **SELL** (expect reversion downward)
- otherwise → **HOLD**

The threshold (±1.5σ) controls sensitivity. Higher thresholds (e.g., ±2.0σ) produce fewer but higher-confidence signals. Lower thresholds (e.g., ±1.0σ) trade more often but with more false positives.

## 3. Project Setup & Tooling

### What is Dune?

Dune is OCaml's build system — equivalent to Cargo (Rust), Leiningen (Clojure), or Webpack (JS). You define your project structure once and `dune build` handles compilation, dependency resolution, and linking automatically.

### Project Structure

```
mean-reverse-bitget/
├── dune-project          # Project metadata
├── lib/
│   ├── dune              # Library dependencies
│   ├── auth.ml           # HMAC-SHA256 signing
│   ├── client.ml         # HTTP wrapper
│   ├── types.ml          # JSON parsers
│   ├── bitget.ml         # API functions
│   ├── strategy.ml       # Mean reversion logic
│   └── trader.ml         # Event loop
├── bin/
│   ├── dune              # Executable config
│   └── main.ml           # Entry point
├── .env                  # API keys (gitignored)
├── .gitignore
└── README.md
```

### Dependencies

Install these via opam:

```bash
opam install cohttp-lwt-unix yojson digestif base64 lwt tls-lwt
```

| Library | Why |
|---|---|
| `cohttp-lwt-unix` | Async HTTP client (GET/POST) |
| `yojson` | Parse and construct JSON |
| `digestif` | HMAC-SHA256 for API signing |
| `base64` | Base64 encode HMAC output |
| `lwt` | Cooperative async/await |
| `tls-lwt` | HTTPS/TLS support |

### The dune-project file

```lisp
(lang dune 3.23)
(name bitget_trader)
```

### The library dune file

```lisp
(library
 (name bitget_lib)
 (libraries cohttp-lwt-unix yojson digestif base64 lwt tls-lwt))
```

### The executable dune file

```lisp
(executable
 (name main)
 (libraries bitget_lib))
```

## 4. Exchange Authentication (HMAC-SHA256)

Every cryptocurrency exchange uses HMAC-SHA256 to authenticate REST API requests. Understanding this is fundamental to any trading bot.

### The Bitget Authentication Flow

For every private API call, you must send four HTTP headers:

| Header | Value |
|---|---|
| `ACCESS-KEY` | Your API key |
| `ACCESS-TIMESTAMP` | Current Unix time in milliseconds |
| `ACCESS-SIGN` | Base64(HMAC-SHA256(secret, timestamp + method + path + body)) |
| `ACCESS-PASSPHRASE` | Your API passphrase |

### The Signing Code

```ocaml
(* lib/auth.ml *)

let timestamp_ms () =
  let open Unix in
  let tv = gettimeofday () in
  string_of_int (int_of_float (tv *. 1000.0))

let sign ~secret ~timestamp ~method_ ~path ~body =
  let message = timestamp ^ String.uppercase_ascii method_ ^ path ^ body in
  let open Digestif.SHA256 in
  hmac_string ~key:secret message |> to_raw_string |> Base64.encode_string

let headers ~api_key ~secret ~passphrase ~method_ ~path ~body =
  let ts = timestamp_ms () in
  let signature = sign ~secret ~timestamp:ts ~method_ ~path ~body in
  [
    ("ACCESS-KEY", api_key);
    ("ACCESS-SIGN", signature);
    ("ACCESS-TIMESTAMP", ts);
    ("ACCESS-PASSPHRASE", passphrase);
  ]
```

Key insight: the `sign` function concatenates `timestamp + METHOD + /path + '{"body":...}'` into a single string, then HMAC-SHA256 hashes it with your secret key, then base64-encodes the raw bytes. The exchange does the same computation on its end and compares.

### The HTTP Client

```ocaml
(* lib/client.ml *)

open Lwt.Infix

let get ~headers uri_str =
  let headers = Cohttp.Header.of_list headers in
  let uri = Uri.of_string uri_str in
  Cohttp_lwt_unix.Client.get ~headers uri
  >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body

let post ~headers ~body uri_str =
  let headers = Cohttp.Header.of_list headers in
  let uri = Uri.of_string uri_str in
  let body_cohttp = Cohttp_lwt.Body.of_string body in
  Cohttp_lwt_unix.Client.post ~headers ~body:body_cohttp uri
  >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body
```

Note the `>>=` operator (pronounced "bind") — it's Lwt's equivalent of JavaScript's `await`. The left side is a promise (`Lwt.t`), the right side is a callback that receives the resolved value.

## 5. Fetching Market Data

### Public vs Private Endpoints

Some endpoints are public (no auth needed): ticker prices, order books, candle history. Others require authentication: balances, order placement, account info.

### The Ticker Endpoint

```ocaml
(* lib/bitget.ml — public endpoints *)

let ticker ~symbol =
  let path = Printf.sprintf "/api/v2/spot/market/tickers?symbol=%s" symbol in
  Client.get ~headers:[] (base_url ^ path)
```

No auth headers required — this is a public endpoint.

### Parsing the Response

```ocaml
(* lib/types.ml — JSON deserialization *)

type ticker_item = {
  symbol : string;
  last_pr : string;
  high_24h : string;
  low_24h : string;
  base_volume : string;
  usdt_volume : string;
}

let parse_ticker_response json_str =
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let code = json |> member "code" |> to_string in
  let msg = json |> member "msg" |> to_string in
  let data =
    json |> member "data" |> to_list |> List.map (fun item ->
      {
        symbol = item |> member "symbol" |> to_string;
        last_pr = item |> member "lastPr" |> to_string;
        high_24h = item |> member "high24h" |> to_string;
        low_24h = item |> member "low24h" |> to_string;
        base_volume = item |> member "baseVolume" |> to_string;
        usdt_volume = item |> member "usdtVolume" |> to_string;
      })
  in
  (code, msg, data)
```

The pipe-forward operator `|>` chains transformations left-to-right. `json |> member "code" |> to_string` reads: take `json`, extract field `"code"`, convert to OCaml string.

### Candles for Historical Data

```ocaml
let candles ~symbol ~granularity ~limit =
  let path =
    Printf.sprintf "/api/v2/spot/market/candles?symbol=%s&granularity=%s&limit=%d"
      symbol granularity limit
  in
  Client.get ~headers:[] (base_url ^ path)
```

We use this to seed the initial price window — fetch 20 one-minute candles, extract close prices, and use them as the starting data for the strategy.

## 6. The Strategy Logic

### Pure Functions: The Heart of Testability

The strategy is a **pure function** — same inputs always produce the same output. No I/O, no global state, no side effects. This makes it trivial to test, debug, and backtest.

```ocaml
(* lib/strategy.ml *)

type signal = Buy | Sell | Hold

let mean_reversion ~prices ~current_price =
  let n = List.length prices in
  if n < 5 then Hold
  else
    let sum = List.fold_left (+.) 0. prices in
    let mean = sum /. float n in
    let variance =
      List.fold_left (fun acc p -> acc +. (p -. mean) ** 2.) 0. prices /. float n
    in
    let stddev = sqrt variance in
    if stddev < 0.01 then Hold
    else
      let z = (current_price -. mean) /. stddev in
      if z < -1.5 then Buy
      else if z > 1.5 then Sell
      else Hold
```

Let's trace through an example. Say our window of 20 close prices has:
- μ = 95.20
- σ = 0.03
- current = 95.15

Then `z = (95.15 − 95.20) / 0.03 = −1.67`

Since `z < −1.5`, the signal is **BUY**. The strategy expects the price to revert upward toward the mean.

### Why This Works

Under normal market conditions, prices oscillate around a local mean within a standard deviation or two. Extreme deviations (z > |2.0|) are statistically rare events that tend to correct. This is the same principle behind Bollinger Bands, where the bands are set at μ ± 2σ.

### The Rolling Window

We can't fetch 20 candles every cycle — that's wasteful. Instead, we maintain a rolling window of close prices, appending each new tick and dropping the oldest when the window exceeds 20:

```ocaml
let append_price state price =
  let prices = price :: state.prices in
  let prices =
    if List.length prices > state.window_size then
      List.rev (List.tl (List.rev prices))
    else prices
  in
  { state with prices; cycle = state.cycle + 1 }
```

`::` prepends to the list (O(1)). When we exceed the window, we drop the last element (the oldest price).

## 7. The Event Loop

### Why an Event Loop?

A trading bot isn't a script that runs once — it's a **daemon** that runs indefinitely, polling data and making decisions. The event loop is the spine of the entire application.

### Structure

```ocaml
(* lib/trader.ml *)

let rec loop ~stop ~api_key ~secret ~passphrase ~symbol state =
  if !stop then begin
    Printf.printf "\n=== Bot stopped after %d cycles ===\n%!" state.cycle;
    Lwt.return ()
  end else
    Bitget.ticker ~symbol >>= fun ticker_json ->
    match Types.parse_ticker_response ticker_json with
    | _, _, [t] ->
      let current_price = float_of_string t.last_pr in
      let state = append_price state current_price in
      let signal =
        Strategy.mean_reversion ~prices:state.prices ~current_price
      in
      let ts = format_ts () in
      Printf.printf "[%s] #%-4d  Price: %6.2f  Window: %2d  Signal: %s\n%!"
        ts state.cycle current_price (List.length state.prices)
        (signal_label signal);
      Lwt_unix.sleep state.interval >>= fun () ->
      loop ~stop ~api_key ~secret ~passphrase ~symbol state

    | code, msg, _ ->
      Printf.eprintf "Ticker error (code=%s msg=%s), retrying...\n%!" code msg;
      Lwt_unix.sleep state.interval >>= fun () ->
      loop ~stop ~api_key ~secret ~passphrase ~symbol state
```

Key design decisions:

1. **Recursive function, not while loop** — OCaml favors recursion. Each call is a tail call (no stack growth). This is equivalent to a `while true` in imperative languages.

2. **`Lwt_unix.sleep`** — yields control to the Lwt scheduler instead of blocking the OS thread. Other Lwt threads can run concurrently during the sleep.

3. **`stop` ref** — a mutable reference checked at the top of every cycle. When SIGINT fires, it's set to true, and the loop exits cleanly.

4. **Error resilience** — if the ticker API returns an unexpected format, we log and retry instead of crashing.

### Graceful Shutdown

```ocaml
let run ~api_key ~secret ~passphrase ~symbol initial_state =
  let stop = ref false in
  let _ = Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    stop := true;
    print_endline ""
  )) in
  loop ~stop ~api_key ~secret ~passphrase ~symbol initial_state
```

When the user presses Ctrl+C:
1. The OS sends SIGINT to the process
2. Our signal handler sets `stop := true`
3. The current Lwt promise resolves (network request or sleep)
4. The next iteration checks `stop`, sees `true`, and exits cleanly
5. A summary is printed

This is the difference between a production bot and a student script. If you Ctrl+C a Python bot mid-API call, you corrupt state. Lwt manages cancellation cleanly.

## 8. Security & Key Management

### Never Hardcode API Keys

This is the #1 mistake in beginner trading bots. Keys in source code get committed to GitHub, scraped by bots, and drained within hours.

**The right way:**

```ocaml
(* .env — gitignored *)
BITGET_API_KEY=bg_your_key
BITGET_SECRET_KEY=your_secret
BITGET_PASSPHRASE=your_passphrase
```

```ocaml
(* bin/main.ml — reads .env at startup *)
let load_env () =
  let chan = open_in ".env" in
  let finally () = close_in chan in
  Fun.protect ~finally (fun () ->
    try
      while true do
        let line = input_line chan in
        match String.split_on_char '=' line with
        | [k; v] -> Unix.putenv k v
        | _ -> ()
      done
    with End_of_file -> ()
  )
```

### .gitignore Protection

```
.env          ← never commit this
_build/       ← build artifacts
*.exe         ← compiled binaries
.DS_Store     ← macOS metadata
```

## 9. Putting It All Together

### The Entry Point

```ocaml
(* bin/main.ml *)

open Bitget_lib

let () =
  (try load_env () with _ -> ());
  let api_key = Unix.getenv "BITGET_API_KEY" in
  let secret = Unix.getenv "BITGET_SECRET_KEY" in
  let passphrase = Unix.getenv "BITGET_PASSPHRASE" in

  (* Initialize price history from 20 candles *)
  let candle_json =
    Lwt_main.run (Bitget.candles ~symbol:"SOLUSDT" ~granularity:"1min" ~limit:20)
  in
  let _, _, candles = Types.parse_candle_response candle_json in
  let close_prices =
    List.rev_map (fun (c : Types.candle) -> float_of_string c.close) candles
  in

  let state =
    Trader.init_state ~prices:close_prices ~window_size:20 ~interval:10.0
  in
  Lwt_main.run (Trader.run ~api_key ~secret ~passphrase ~symbol:"SOLUSDT" state)
```

### Running the Bot

```bash
dune exec bin/main.exe
```

Example output:

```
=== Bitget Mean Reversion Bot ===
Fetching initial price window (20 candles)...
Loaded 20 prices (95.06 .. 95.28)
Starting event loop (press Ctrl+C to stop)...

[16:31:58] #1     Price:  95.06  Window: 20  Signal: BUY  ↓
[16:32:09] #2     Price:  95.06  Window: 20  Signal: BUY  ↓
[16:32:19] #3     Price:  95.03  Window: 20  Signal: BUY  ↓
[16:32:29] #4     Price:  95.05  Window: 20  Signal: BUY  ↓
^C
=== Bot stopped after 4 cycles ===
```

### The Full Data Flow

```
┌─────────────┐     ┌──────────────┐     ┌─────────────┐
│  Bitget API │────▶│  lib/auth.ml │────▶│ lib/client.ml│
│  (REST)     │     │  (signing)   │     │ (HTTP I/O)  │
└─────────────┘     └──────────────┘     └──────┬──────┘
                                                │
                                                ▼
┌─────────────┐     ┌──────────────┐     ┌─────────────┐
│ bin/main.ml │◀────│ lib/types.ml │◀────│ lib/bitget.ml│
│ (entry)     │     │ (parsing)    │     │ (API calls) │
└──────┬──────┘     └──────────────┘     └─────────────┘
       │
       ▼
┌─────────────┐     ┌──────────────┐     ┌─────────────┐
│ lib/trader.ml│────▶│lib/strategy.ml│    │  Your Screen│
│ (loop)      │     │(computation) │     │  (output)   │
└─────────────┘     └──────────────┘     └─────────────┘
```

## 10. Next Steps

This bot is a foundation. Here's what you can build on top of it:

### Real Order Execution
The `Bitget.place_limit_order` function is already implemented. Wire it into the `trader.ml` signal handler to place actual trades. Add position tracking so you don't double-buy.

### WebSocket Feed
Replace REST polling with Bitget's WebSocket streams for sub-100ms price updates. The strategy becomes event-driven instead of poll-driven.

### Backtesting
Feed historical candle data through the strategy function and simulate trades. Measure Sharpe ratio, max drawdown, win rate — before risking real capital.

### Risk Management
Add position sizing (fixed % of portfolio), stop-loss orders, daily loss limits, and cooldown periods after trades.

### Multi-Pair
The strategy functions are parameterized by `~symbol`. Run the loop for multiple pairs concurrently using Lwt's `Lwt.join` or `Lwt.pick`.

### Alerting
Send signals to Telegram, Discord, or email using a simple HTTP POST to their webhook APIs.

---

*Built with OCaml, Dune, Lwt, Cohttp, Digestif, and Yojson.*

*The complete source is at [github.com/yllvar/mean-reverse-bitget](https://github.com/yllvar/mean-reverse-bitget).*
