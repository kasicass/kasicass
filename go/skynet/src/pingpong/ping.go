package pingpong

import (
	"fmt"
	"time"

	"skynet"
)

type PingService struct {
}

func (self *PingService) Call(me *skynet.Planet, method string, params string) {
	switch method {
	case "init":
		fmt.Println("[Ping] init")
		time.Sleep(5)
		fmt.Println("[Ping] call Pong")
		me.ToUniverse("Pong", "ping", "ping-params")

	case "pong":
		fmt.Println("[Ping] recv", params)

	default:
		fmt.Println("[Ping] error method", method)
	}
}

