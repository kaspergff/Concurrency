start cmd.exe /k "stack run -- 1100 1102 1101"
start cmd.exe /k "stack run -- 1101 1100 1102"

start cmd.exe /k "stack run -- 1104 1102 1105"
start cmd.exe /k "stack run -- 1105 1104 1106"
start cmd.exe /k "stack run -- 1106 1105 1102"
ping 192.0.2.2 -n 1 -w 4000 > nul
start cmd.exe /k "stack run -- 1102 1100 1101 1106 1104"