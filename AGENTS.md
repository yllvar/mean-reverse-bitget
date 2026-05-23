# AGENTS.md — ocaml-bitget

## Quick start
```bash
cp .env.example .env   # fill in Bitget API credentials
dune exec bin/main.exe
```

## Build & run
- **Build:** `dune build`
- **Run live bot:** `dune exec bin/main.exe`
- **Run backtest:** `dune exec bin/backtest.exe` (fetches 1000 candles, runs 64-combo grid)
- No tests, no linter, no formatter, no CI, no pre-commit hooks.

## Architecture
Single `lib/bitget_lib` library consumed by `bin/main.exe` and `bin/backtest.exe`.
| File | Role |
|---|---|
| `lib/auth.ml` | HMAC-SHA256 signing, ACCESS-* header construction |
| `lib/client.ml` | Lwt+Cohttp GET/POST wrappers |
| `lib/types.ml` | Yojson deserializers for all API responses |
| `lib/bitget.ml` | Bitget REST API surface (v2) |
| `lib/strategy.ml` | Pure-function mean reversion: z-score ±threshold, trend filter via SMA |
| `lib/trader.ml` | Recursive Lwt event loop, SIGINT shutdown, virtual P&L, CSV signal output |
| `bin/main.ml` | Loads `.env`, seeds 220 candles (20 z-window + 200 SMA), runs loop |
| `bin/backtest.ml` | Offline backtest: parameter grid (window × threshold × sma_period), equity CSV |

## Key details
- **No test framework** — no `dune test` target, no test directory.
- **No formatter** — no `.ocamlformat`. Match existing style (4-space indent, no comments).
- **Libraries** (all opam): `cohttp-lwt-unix yojson digestif base64 lwt tls-lwt`
- **`.env` is gitignored** but loaded at runtime via `Unix.putenv` line parser in `main.ml`.
- **Library name**: `bitget_lib` — used in `bin/dune` as `(libraries bitget_lib)`.
- **Project name**: `bitget_trader` (in `dune-project`).
- **SOL/USDT spot** hardcoded, 10s polling, 20-price z-score window, 200-price SMA trend filter.
- **Order placement** (`Bitget.place_limit_order`) is implemented but not wired into the event loop.
- **Strategy**: `mean_reversion` takes `~threshold` parameter; `apply_trend_filter` suppresses counter-trend signals.
- **Live P&L**: virtual $1000 portfolio tracks hypothetical trades with 0.1% fee.
- **CSV output**: each tick emits `SIGNAL,unix_ts,price,z,signal,window` to stdout.
- **Dockerfile**: multi-stage build (ocaml/opam → alpine), includes `gmp` runtime dep.
- **Railway**: deployed as `ocaml-bitget-bot`, env vars set via Railway dashboard.
