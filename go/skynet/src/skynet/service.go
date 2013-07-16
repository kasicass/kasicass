package skynet

type Service interface {
	Call(me *Planet, method string, params string)
}

