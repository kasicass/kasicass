package com.juvenxu.mvnbook.hellomaven;

public class HelloMaven {
    public String sayHello() {
        return "Hello Maven";
    }
    
    public static void main(String[] args) {
        System.out.println(new HelloMaven().sayHello());
    }
}