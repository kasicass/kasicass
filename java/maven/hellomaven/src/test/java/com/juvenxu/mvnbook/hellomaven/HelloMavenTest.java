package com.juvenxu.mvnbook.hellomaven;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class HelloMavenTest {
    @Test
    public void testSayHello() {
        HelloMaven hello = new HelloMaven();
        String result = hello.sayHello();
        assertEquals("Hello Maven", result);
    }
}