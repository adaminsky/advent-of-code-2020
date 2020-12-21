with open('day2_in.txt', 'r') as f:
    numRight = 0
    lines = f.read().split("\n")
    for line in lines:
        if line == "":
            continue

        line = line.split()
        interval = line[0].split("-")
        letter = line[1]
        letter = letter[:-1]
        password = line[2]

        occurence = 0
        for c in password:
            if c == letter:
                occurence += 1
        if occurence >= int(interval[0]) and occurence <= int(interval[1]):
            numRight += 1
    print(numRight)

    numRight = 0
    for line in lines:
        if line == "":
            continue

        line = line.split()
        interval = line[0].split("-")
        letter = line[1]
        letter = letter[:-1]
        password = line[2]

        occurence = 0
        c1 = password[int(interval[0]) - 1] == letter
        c2 = password[int(interval[1]) - 1] == letter
        if c1 is not c2:
            numRight += 1
    print(numRight)
