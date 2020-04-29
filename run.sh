#!/bin/bash
ghc --make Main || exit 1
mkdir -p generationOutputs # Create folder if it does not exitst
mkdir -p generationSolution # Create folder if it does not exitst
rm generationOutputs/*
rm generationSolution/*
./Main || exit 1
# Video Generation:
rm mazeGeneration.mp4
rm mazePathFinding.mp4
ffmpeg -r 20 -i generationOutputs/mazeGen_%d.svg -c:v libx264 -vf fps=25 -pix_fmt yuv420p mazeGeneration.mp4
mkfifo temp1 temp2
totalSolutions=$(ls generationSolution | wc -l)
rateSolution=$(expr $totalSolutions \* 15 / 720)
if [ $rateSolution -le 300 ]
then
    rateSolution=6
fi
ffmpeg -y -r $rateSolution -i generationSolution/mazeSol_%d.svg -c:v libx264 -vf fps=25 -pix_fmt yuv420p -f mpegts temp1 2> /dev/null & \
ffmpeg -y -loop 1 -i maze_solution.svg -c:v libx264 -t 4 -pix_fmt yuv420p -f mpegts temp2 2> /dev/null & \
ffmpeg -f mpegts -i "concat:temp1|temp2" -c copy -bsf:a aac_adtstoasc mazePathFinding.mp4

# function ProgressBar {
# # Process data
#     let _progress=(${1}*100/${2}*100)/100
#     let _done=(${_progress}*4)/10
#     let _left=40-$_done
# # Build progressbar string lengths
#     _fill=$(printf "%${_done}s")
#     _empty=$(printf "%${_left}s")

# # 1.2 Build progressbar strings and print the ProgressBar line
# # 1.2.1 Output example:                           
# # 1.2.1.1 Progress : [########################################] 100%
# printf "\rProgress : [${_fill// /#}${_empty// /-}] ${_progress}%%"

# }

# # Variables
# _start=1

# # This accounts as the "totalState" variable for the ProgressBar function
# _end=100

# # Proof of concept
# for number in $(seq ${_start} ${_end})
# do
#     sleep 0.1
#     ProgressBar ${number} ${_end}
# done
# printf '\nFinished!\n'