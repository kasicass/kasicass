package mycode.discard;

import io.netty.buffer.ByteBuf;

import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;

public class DiscardServerHandler extends ChannelInboundHandlerAdapter {
	@Override
	public void channelRead(ChannelHandlerContext ctx, Object msg) {
		((ByteBuf)msg).release();
	}

	@Override
	public void handlerAdded(ChannelHandlerContext ctx) {
		System.out.println("handlerAdded");
	}

	@Override
	public void handlerRemoved(ChannelHandlerContext ctx) {
		System.out.println("handlerRemoved");
	}

	@Override
	public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
		cause.printStackTrace();
		ctx.close();
	}
}
