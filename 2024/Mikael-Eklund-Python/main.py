import importlib
import time
from typing import Any

def run_day(day: int) -> tuple[Any, Any]:
    """
    Dynamically import and run a specific day's solution.
    
    Args:
        day (int): Day number to run
    
    Returns:
        tuple: Results of part 1 and part 2
    """
    try:
        # Dynamically import the day's module
        module = importlib.import_module(f'advent_of_code.solutions.day{day:02d}')
        
        # Read input file
        with open(f'inputs/day{day:02d}.txt', 'r') as f:
            #input_data = f.read().strip()
            input_data = f.read().split("\n")
        
        # Time the solutions
        start_time = time.time()
        part1_result = module.part1(input_data)
        part1_time = time.time() - start_time
        
        start_time = time.time()
        part2_result = module.part2(input_data)
        part2_time = time.time() - start_time
        
        print(f"Day {day:02d} Results:")
        print(f"Part 1: {part1_result} (Time: {part1_time:.4f}s)")
        print(f"Part 2: {part2_result} (Time: {part2_time:.4f}s)")
        
        return part1_result, part2_result
    
    except ImportError:
        print(f"No solution found for Day {day}")
        return None, None
    except FileNotFoundError:
        print(f"Input file for Day {day} not found")
        return None, None

def main():
    # Run a specific day or all days
    run_day(4)  # Example: run day 1
    # Alternatively, you could loop through days here

if __name__ == "__main__":
    main()