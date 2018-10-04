package main

import (
	"fmt"
	"net"
	"os"
)

func main() {
	ifaces, err := net.Interfaces()
	if err != nil {
		fmt.Fprintf(os.Stderr, "net.Interfaces() error: %s\n", err.Error())
		os.Exit(1)
	}
	for _, i := range ifaces {
		fmt.Printf("interface: %s\n", i.Name)
		addrs, _ := i.Addrs()
		for _, addr := range addrs {
			var ip net.IP
			switch v := addr.(type) {
			case *net.IPNet:
				ip = v.IP
			case *net.IPAddr:
				ip = v.IP
			}
			fmt.Printf("  addr: %s\n", ip)
		}
	}
}
