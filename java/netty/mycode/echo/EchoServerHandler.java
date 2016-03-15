package mycode.echo;

import java.nio.charset.Charset;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelHandler.Sharable;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.channel.ChannelFutureListener;

@Sharable
public class EchoServerHandler extends ChannelInboundHandlerAdapter {

	// called for each incoming message
	@Override
	public void channelRead(ChannelHandlerContext ctx, Object msg) {
		ByteBuf in = (ByteBuf) msg;
		System.out.println("Server recv: " + in.toString(Charset.forName("UTF-8")));
		ctx.write(in);
	}

	// called to notify the handler that the last call made to channelRead() was
	// the last message in the current batch
	@Override
	public void channelReadComplete(ChannelHandlerContext ctx) {
		ctx.writeAndFlush(Unpooled.EMPTY_BUFFER).addListener(ChannelFutureListener.CLOSE);
	}

	// called if an exception is thrown during the read operation
	@Override
	public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
		cause.printStackTrace();
		ctx.close();
	}
}
