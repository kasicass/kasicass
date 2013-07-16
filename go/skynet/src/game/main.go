package main

import (
	"skynet"
	"pingpong"
)

func main() {
	uni := skynet.MakeUniverse("")
	uni.RegisterService("Ping", &pingpong.PingService{})
	uni.RegisterService("Pong", &pingpong.PongService{})
	uni.Run()
}

