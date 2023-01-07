#!/bin/bash

#Run script for Einstein

./diffusion << EOF 
NVEtrj.gro
221
4
50000
0.002
1
10
100
EOF

#Run script for Green-Kubo

./diffusion << EOF 
NVEtrj.gro
221
4
50000
0.002
2
EOF

