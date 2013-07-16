package pingpong

import (
	"fmt"

	"skynet"
)

type PongService struct {
}

func (self *PongService) Call(me *skynet.Planet, method string, params string) {
	switch method {
	case "init":
		fmt.Println("[Pong] init")

	case "ping":
		fmt.Println("[Pong] recv", params)
		fmt.Println("[Pong] echo Ping")
		me.ToUniverse("Ping", "pong", "pong-params")

	default:
		fmt.Println("[Pong] error method", method)
	}
}

