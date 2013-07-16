package skynet

type uniMessage struct {
	toPlanet string
	method string
	params string
}

type Universe struct {
	inque chan *uniMessage
	planets map[string] *Planet
}

func MakeUniverse(conf string) *Universe {
	inque := make(chan *uniMessage, 0)
	planets := make(map[string] *Planet)
	return &Universe{inque, planets}
}

func (self *Universe) RegisterService(name string, svc Service) {
	p := makePlanet(svc)
	self.planets[name] = p
	go p.run(self.inque)
	p.call("init", "")
}

func (self *Universe) Run() {
	for {
		select {
		case msg, ok := <-self.inque:
			if !ok {
				// chan close ?
			} else {
				p, ok := self.planets[msg.toPlanet]
				if ok {
					p.call(msg.method, msg.params)
				}
			}
		}
	}
}

