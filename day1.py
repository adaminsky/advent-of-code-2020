with open('day1_in.txt', 'r') as f:
    lines = f.read()
    lines = lines.split("\n")
    cnt = len(lines)
    for i in range(cnt):
        for j in range(i+1, cnt):
            if lines[i] == '' or lines[j] == '':
                continue
            if int(lines[i]) + int(lines[j]) == 2020:
                print(int(lines[i]) * int(lines[j]))

    for i in range(cnt):
        for j in range(i+1, cnt):
            for k in range(j+1, cnt):
                if lines[i] == '' or lines[j] == '' or lines[k] == '':
                    continue
                if int(lines[i]) + int(lines[j]) + int(lines[k]) == 2020:
                    print(int(lines[i]) * int(lines[j]) * int(lines[k]))
