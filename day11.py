def blink(stone: str) -> list[str]:
    if stone == "0":
        return ["1"]
    if len(stone) % 2 == 0:
        mid = len(stone)//2
        return [stone[:mid], str(int(stone[mid:]))]
    return [str(int(stone) * 2024)]


def count_stones(blinks: int, stones: list[str]) -> int:
    counts = {}
    for stone in stones:
        counts[stone] = counts.get(stone, 0) + 1
    for _ in range(blinks):
        new_counts = {}
        for stone, count in counts.items():
            for snew_stone in blink(stone):
                new_counts[snew_stone] = new_counts.get(snew_stone, 0) + count
        counts = new_counts
    return sum(counts.values())


def main():
    stones = ["0", "44", "175060", "3442", "593", "54398", "9", "8101095"]
    print(f"Task 2: {count_stones(75, stones)}")


if __name__ == "__main__":
    main()
