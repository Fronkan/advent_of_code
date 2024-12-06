import sys
import importlib

def main():
    if len(sys.argv) < 3 or len(sys.argv) > 3 : 
        print("Usage: python main.py <day> <a/b>")
        return

    day = sys.argv[1].zfill(2)
    part = sys.argv[2].lower()
    module_name = f"days.day{day}"

    filePath = f"inputs/day{day}.txt"
    day_module = importlib.import_module(module_name)

    result = day_module.start(filePath, part)

    print(result)

if __name__ == "__main__":
    main()