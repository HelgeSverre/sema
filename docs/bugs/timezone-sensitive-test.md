# Timezone-Sensitive Timestamp Test

## Problem

`test_time_parse` parses the string `"2024-01-15 12:30:00"` and asserts the resulting Unix timestamp falls within a specific range. However, the parsed timestamp depends on the machine's local timezone setting. A machine in UTC will produce a different Unix timestamp than one in US/Pacific for the same local-time string. The test uses a range check to accommodate some variation, but the range may not cover all timezones (e.g., UTC+14 vs UTC-12 spans a 26-hour difference).

This test will pass on most developer machines in similar timezones but can fail in CI environments configured with different timezone settings, or on machines in significantly different timezones.

## Affected Tests

- `crates/sema/tests/integration_test.rs:2353` -- `test_time_parse` -- asserts Unix timestamp range for a local-time string without specifying timezone

## Suggested Fix

1. Use a UTC-explicit time string (e.g., `"2024-01-15T12:30:00Z"`) so the result is timezone-independent.
2. If testing local-time parsing is intentional, compute the expected timestamp dynamically using the same timezone the runtime uses, rather than hardcoding a range.
3. Alternatively, widen the range to cover all possible timezones (roughly +/- 13 hours from UTC), though this makes the assertion weaker.
