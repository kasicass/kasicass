package main

import (
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"os"
	"io/ioutil"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Usage:", os.Args[0], "host:port")
		os.Exit(1)
	}
	service := os.Args[1]

	certPemFile, err := os.Open("jan.newmarch.name.pem")
	checkError(err)
	pemBytes, err := ioutil.ReadAll(certPemFile)
	checkError(err)
	certPemFile.Close()

	certPool := x509.NewCertPool()
	ok := certPool.AppendCertsFromPEM(pemBytes)
	if !ok {
		fmt.Println("PME read failed")
	} else {
		fmt.Println("PEM read ok")
	}

	conn, err := tls.Dial("tcp", service, &tls.Config{RootCAs: certPool})
	checkError(err)

	for n := 0; n < 10; n++ {
		fmt.Println("Writing...")
		conn.Write([]byte("Hello " + string(n+'A')))

		var buf [512]byte
		n, err := conn.Read(buf[0:])
		checkError(err)

		fmt.Println(string(buf[0:n]))
	}
	conn.Close()
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s\n", err.Error())
		os.Exit(1)
	}
}

