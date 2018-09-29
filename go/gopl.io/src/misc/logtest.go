package main

import (
	log "github.com/sirupsen/logrus"
)

func main() {
	log.WithFields(log.Fields{
		"animal": "cat",
		"number": 1,
	}).Info("A walrus appears")
}
