from collections import Counter

def areThereNoDuplicates(col):
    return len([k for k, v in Counter(col).items() if v > 1]) == 0

value = "14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4"
numbers = list(map(int, [x for x in value.split(' ')]))

configs =[]

cycles = 0
while areThereNoDuplicates(configs):
    largest = max(numbers)
    index = [i for i,x in enumerate(numbers) if x == largest][0]

    freeMemory = numbers[index]
    numbers[index] = 0
    index += 1

    while freeMemory > 0:
        numbers[index % len(numbers)] += 1
        freeMemory -= 1
        index += 1

    configs.append(''.join(map(str, numbers)))
    cycles += 1



print(cycles)