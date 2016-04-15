package mycode.bytebuf;

import java.nio.charset.Charset;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;

public class MyBufTest {
    public static void main(String[] args) {
        Charset utf8 = Charset.forName("UTF-8");
        ByteBuf buf = Unpooled.copiedBuffer("Netty in Action rocks!", utf8);
        System.out.println((char)buf.readByte());
        
        System.out.println("readerIndex: " + buf.readerIndex());
        System.out.println("writerIndex: " + buf.writerIndex());
        System.out.println("capacity: " + buf.capacity());
        System.out.println("maxCapacity: " + buf.maxCapacity());
        System.out.println("hasArray: " + buf.hasArray());
    }
}