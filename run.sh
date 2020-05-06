#!/bin/bash
ghc --make Main || exit 1
mkdir -p generationOutputs # Create folder if it does not exitst
mkdir -p generationSolution # Create folder if it does not exitst
rm generationOutputs/*
rm generationSolution/*
./Main || exit 1
# Video Generation:
rm animations/mazeGeneration.mp4
rm animations/mazeGeneration.gif
rm animations/mazePathFinding.mp4
rm animations/mazePathFinding.gif
mkfifo temp1 temp2
ffmpeg -y -r 20 -i generationOutputs/mazeGen_%d.svg -c:v libx264 -vf fps=25 -pix_fmt yuv420p -f mpegts temp1 2> /dev/null & \
ffmpeg -y -loop 1 -i maze.svg -c:v libx264 -t 4 -pix_fmt yuv420p -f mpegts temp2 2> /dev/null & \
ffmpeg -f mpegts -i "concat:temp1|temp2" -c copy -bsf:a aac_adtstoasc animations/mazeGeneration.mp4
totalSolutions=$(ls generationSolution | wc -l)
rateSolution=$(expr $totalSolutions \* 15 / 720)
if [ $totalSolutions -le 300 ]
then
    rateSolution=6
fi
ffmpeg -y -r $rateSolution -i generationSolution/mazeSol_%d.svg -c:v libx264 -vf fps=25 -pix_fmt yuv420p -f mpegts temp1 2> /dev/null & \
ffmpeg -y -loop 1 -i maze_solution.svg -c:v libx264 -t 4 -pix_fmt yuv420p -f mpegts temp2 2> /dev/null & \
ffmpeg -f mpegts -i "concat:temp1|temp2" -c copy -bsf:a aac_adtstoasc animations/mazePathFinding.mp4
# Gif Generation:
cd animations
ffmpeg -i mazePathFinding.mp4 -f gif mazePathFinding.gif
ffmpeg -i mazeGeneration.mp4 -f gif mazeGeneration.gif
