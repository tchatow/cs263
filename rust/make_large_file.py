with open('testfile', 'w') as fout:
    for i in range(32):
        fout.write("A" * 0x400000)
        fout.write("needle");

