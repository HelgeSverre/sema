#!/usr/bin/env python3
"""Generate deterministic test data for 1BRC benchmark.
Uses a fixed seed so all runs use identical data.
Usage: python3 generate-test-data.py <num_rows> <output_file>
"""
import sys
import random

STATIONS = [
    ("Abha", 18.2, 6.3), ("Accra", 26.4, 2.3), ("Addis Ababa", 16.0, 5.7),
    ("Adelaide", 17.3, 6.2), ("Algiers", 18.1, 6.4), ("Amsterdam", 10.3, 6.8),
    ("Ankara", 12.0, 7.4), ("Baghdad", 22.8, 7.9), ("Bangkok", 28.5, 1.7),
    ("Barcelona", 16.2, 5.8), ("Beijing", 12.8, 9.3), ("Berlin", 10.1, 7.2),
    ("Bogota", 13.5, 1.8), ("Brasilia", 21.6, 4.2), ("Brussels", 10.4, 6.7),
    ("Bucharest", 10.7, 8.2), ("Budapest", 11.3, 7.7), ("Buenos Aires", 17.4, 6.4),
    ("Cairo", 21.8, 5.7), ("Cape Town", 16.5, 4.8), ("Chicago", 9.8, 9.4),
    ("Copenhagen", 8.7, 6.6), ("Dallas", 19.0, 7.8), ("Delhi", 25.1, 7.4),
    ("Dublin", 9.8, 5.1), ("Helsinki", 5.8, 8.3), ("Hong Kong", 23.6, 4.2),
    ("Istanbul", 14.0, 6.5), ("Jakarta", 27.2, 1.5), ("Kuala Lumpur", 27.8, 1.4),
    ("Lima", 19.3, 3.1), ("London", 11.5, 6.0), ("Moscow", 5.7, 9.7),
    ("Mumbai", 27.5, 2.9), ("Nairobi", 17.5, 3.8), ("Oslo", 6.3, 7.8),
    ("Paris", 12.0, 6.5), ("Stockholm", 7.3, 7.5), ("Sydney", 17.8, 5.5),
    ("Tokyo", 15.7, 6.6),
]

def main():
    if len(sys.argv) < 3:
        print("Usage: python3 generate-test-data.py <num_rows> <output_file>")
        sys.exit(1)

    num_rows = int(sys.argv[1])
    output_file = sys.argv[2]
    rng = random.Random(42)  # Fixed seed for reproducibility

    with open(output_file, 'w') as f:
        for _ in range(num_rows):
            name, mean, var = rng.choice(STATIONS)
            temp = rng.uniform(mean - var, mean + var)
            f.write(f"{name};{temp:.1f}\n")

    print(f"Generated {num_rows} rows to {output_file}")

if __name__ == "__main__":
    main()
