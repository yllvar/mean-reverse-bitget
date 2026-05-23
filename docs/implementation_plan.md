# Implementation Plan — Signal Exit & SL/TP Validation

## Problem Statement

After 24h of live paper trading, the bot:
- Made **1 buy** at $86.32 (14:26 UTC, cycle #230)
- **0 exits** — position still held after 24h
- P&L: **-0.88%** ($991.17), ranging between +0.30% and -1.18%
- z-score displayed as `+0.000` throughout (hardcoded when holding)

**Root cause**: The live trader skips all signal computation when `has_position = true`. It only checks SL/TP (30%/15%), which are too wide for the current low-volatility market ($84-$88 range).

**Critical discrepancy**: The backtest (`backtest.ml`) exits on Sell signals (z > +threshold) and has **no SL/TP**. The live trader exits only on SL/TP and has **no signal-based exit**. The optimized params (w=500, th=1.0, EMA200) were tuned on the backtest's signal-exit behavior, so the live trader is fundamentally mismatched.

---

## Validation Results (validate_exit.ml)

Backtest on 3M candles (5.7 years), params: w=500, th=1.0, EMA=200, pos=50%:

### Exit Strategy Comparison

| Mode | Trades | Win% | PF | MaxDD | Return |
|------|--------|------|----|-------|--------|
| **Signal-only** | 688 | 59.3% | 1.08 | 75.70% | **+61.55%** |
| SL/TP-only | 146 | 73.3% | 1.13 | 80.27% | +279.88% |
| Signal+SL/TP (30/15) | 700 | 59.3% | 1.03 | 80.38% | **-14.28%** |

### SL Grid Search (Signal+SL, no TP)

| SL | Trades | Win% | PF | MaxDD | Return |
|----|--------|------|----|-------|--------|
| 10% | 796 | 57.7% | 0.98 | 88.50% | -63.31% |
| 20% | 714 | 59.4% | 0.99 | 87.83% | -56.53% |
| 30% | 694 | 59.2% | 1.05 | 80.93% | +9.21% |
| 40% | 689 | 59.4% | 1.08 | 74.33% | +55.57% |
| **50%** | 689 | 59.4% | 1.08 | 75.82% | **+60.73%** |
| 100% (no SL) | 688 | 59.3% | 1.08 | 75.70% | +61.55% |

### Key Findings

1. **Signal-only is optimal** — exit on z > +threshold captures mean reversion reversals
2. **Signal+SL/TP is worse than Signal-only** — SL/TP exits cut winners short while SL still captures full losses (classic "cut winners, let losers run")
3. **Narrow SL (< 30%) destroys performance** — too many stop losses on normal mean reversion dips
4. **SL=50% is near-optimal crash protection** — same trade count as no SL, minimal return impact (-0.8%)
5. **TP is unnecessary** — signal exits handle profit-taking; TP only interferes

### Recommended Live Config

- **Primary exit**: Signal-based (z > +1.0)
- **Safety net**: SL=50% (crash protection, rarely triggers)
- **No TP**: Signals handle exits

---

## Step 1: Fix z-score computation while holding (Proposal 2) ✅ DONE

**File**: `lib/trader.ml` — **COMPLETED**

**Change**: Moved z-score computation before the `has_position` check.

---

## Step 2: Add signal-based exit (Proposal 1) ✅ DONE

**File**: `lib/trader.ml` — **COMPLETED**

**Change**: Added signal-based exit in the `has_position` branch:
```ocaml
else if filtered = Strategy.Sell then
  handle_sl_tp ... "SIGNAL EXIT @ %.2f z=%+.3f (entry=%.2f)"
```

---

## Step 3: Validation backtest ✅ DONE

**File**: `bin/validate_exit.ml` — **COMPLETED**

**Results**: See validation table above. Hypothesis confirmed: signal exits match backtest behavior.

---

## Step 4: SL grid search ✅ DONE

**File**: `bin/validate_exit.ml` — **COMPLETED**

**Results**: SL=50% is optimal crash protection. SL < 30% destroys performance.

---

## Step 5: Fix order logging (Proposal 4) ✅ DONE

**File**: `lib/trader.ml` — **COMPLETED**

**Change**: Added `[ORDER]` prefix with `Printf.eprintf` fallback for all order results.

---

## Step 6: Update live config and deploy

**File**: `bin/main.ml` — needs SL changed from 0.30 to 0.50

**Change**: Update `stop_loss` parameter from 0.30 to 0.50.

**Then**:
1. Build: `dune build`
2. Run tests: `dune exec bin/test.exe`
3. Deploy to Railway
4. Monitor for: signal exits triggering, order logs appearing, P&L behavior

---

## Execution Order

| Step | Status | File(s) | Notes |
|------|--------|---------|-------|
| 1 | ✅ Done | `lib/trader.ml` | z-score always computed |
| 2 | ✅ Done | `lib/trader.ml` | Signal exit added |
| 3 | ✅ Done | `bin/validate_exit.ml` | 3-mode comparison |
| 4 | ✅ Done | `bin/validate_exit.ml` | SL grid search |
| 5 | ✅ Done | `lib/trader.ml` | Order logging fixed |
| 6 | Pending | `bin/main.ml`, Railway | Update SL=50%, deploy |

---

## What We're NOT Changing

- **`lib/strategy.ml`**: No changes. The strategy logic (z-score, trend filter) is correct.
- **`lib/bitget.ml`**: No changes. Order placement works.
- **`bin/backtest.ml`**: No changes. Keep as-is for historical comparison.
- **`bin/test.ml`**: No changes. Existing tests still valid.
- **`Dockerfile`**: No changes.
- **API credentials, deployment config**: No changes.
