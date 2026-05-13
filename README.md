# Mean Reverse Bitget

An OCaml mean reversion trading bot for the Bitget exchange (SOL/USDT spot). Built with Dune, Lwt, Cohttp, and Digestif.

## Architecture

```
lib/
├── auth.ml       HMAC-SHA256 signing + header construction
├── client.ml     Lwt-based HTTP GET/POST (cohttp-lwt-unix)
├── types.ml      JSON deserialization (yojson) for all API responses
├── bitget.ml     API surface: balance, ticker, orderbook, candles, place_order
├── strategy.ml   Mean reversion signal: z-score ±1.5σ on rolling price window
└── trader.ml     Continuous event loop with graceful shutdown (SIGINT)

bin/
└── main.ml       Entry point: loads .env → initialises price window → runs loop
```

## Setup

Requires OCaml ≥ 4.14 with opam and dune.

```bash
git clone git@github.com:yllvar/mean-reverse-bitget.git
cd mean-reverse-bitget
cp .env.example .env    # fill in your Bitget API credentials
dune exec bin/main.exe
```

## Configuration

Create a `.env` file (gitignored, never commit it):

```
BITGET_API_KEY=bg_your_key
BITGET_SECRET_KEY=your_secret
BITGET_PASSPHRASE=your_passphrase
```

## How It Works

1. Fetches 20 one-minute candles to build an initial price window
2. Polls the SOL/USDT ticker every 10 seconds
3. Maintains a rolling window of the last 20 close prices
4. Computes z-score: `(current_price − mean) / stddev`
5. **BUY** when z < −1.5 (price abnormally low)
6. **SELL** when z > +1.5 (price abnormally high)
7. **HOLD** otherwise

## Dependencies

| Library | Purpose |
|---|---|
| `cohttp-lwt-unix` | HTTP client |
| `yojson` | JSON parsing |
| `digestif` | HMAC-SHA256 |
| `base64` | Base64 encoding |
| `lwt` | Async concurrency |
| `tls-lwt` | TLS/HTTPS support |

## Development

```bash
dune build       # compile
dune exec bin/main.exe   # run
```
