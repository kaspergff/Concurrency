start cmd.exe /k "stack run -- 1100 1102"
start cmd.exe /k "stack run -- 1101 1102"
ping 192.0.2.2 -n 1 -w 100 > nul
start cmd.exe /k "stack run -- 1102 1100 1101"
