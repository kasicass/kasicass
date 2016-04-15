
INode 是场景的一个逻辑节点，一个场景就是一个 root node，然后里面每一个 object（比如 box, sphere）则是 scene node 的 child node。

ObjectState INode::GetWorldState()
返回一个 node's pipeline 处理完成之后的 object，我的理解，就是 init node 经过了所有扭曲、形变之后的处理，得到的新的 node



自己实现的插件中，如果希望从 3D Max 的格式（比如 TriObj）转换成为自定义的格式，可以实现一个 ObjectConverter 的类，由其进行转换工作。

TM, transfer matrix
TSM, world space modifier
