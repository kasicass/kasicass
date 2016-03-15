package mycode.echo;

import java.nio.charset.Charset;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelHandler.Sharable;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.SimpleChannelInboundHandler;

@Sharable
public class EchoClientHandler extends SimpleChannelInboundHandler<ByteBuf> {
	Charset UTF_8 = Charset.forName("UTF-8");
	
	@Override
	public void channelActive(ChannelHandlerContext ctx) {
		ctx.writeAndFlush(Unpooled.copiedBuffer("Netty rocks", UTF_8));
	}

	@Override
	public void channelRead0(ChannelHandlerContext ctx, ByteBuf in) {
		System.out.println("Client recv: " + in.toString(UTF_8));
	}

	@Override
	public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
		cause.printStackTrace();
		ctx.close();
	}
}

