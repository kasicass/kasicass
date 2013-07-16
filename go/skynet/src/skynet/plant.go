package skynet

type callMessage struct {
	method string
	params string
}

type Planet struct {
	svc Service
	inque chan *callMessage
	outque chan<- *uniMessage
}

func makePlanet(svc Service) *Planet {
	inque := make(chan *callMessage, 0)
	return &Planet{svc, inque, nil}
}

func (self *Planet) call(method string, params string) {
	self.inque <- &callMessage{method, params}
}

// wait/recv, do sth, reply to Universe
func (self *Planet) run(outque chan<- *uniMessage) {
	self.outque = outque
	for {
		select {
		case msg, ok := <-self.inque:
			if !ok {
				// chan close
				break
			} else {
				self.svc.Call(self, msg.method, msg.params)
			}
		}
	}
}

func (self *Planet) ToUniverse(toPlanet string, method string, params string) {
	self.outque <- &uniMessage{toPlanet, method, params}
}

