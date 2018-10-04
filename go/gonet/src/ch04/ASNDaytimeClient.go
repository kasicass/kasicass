package main

import (
	"encoding/asn1"
	"fmt"
	"os"
	"net"
	"io/ioutil"
	"time"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "Usage: %s host:port", os.Args[0])
	}

	service := os.Args[1]
	conn, err := net.Dial("tcp", service)
	checkError(err)

	mdata, err := ioutil.ReadAll(conn)

	var newtime time.Time
	_, err1 := asn1.Unmarshal(mdata, &newtime)
	checkError(err1)

	fmt.Println("After marshal/unmarshal:", newtime.String())
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}
