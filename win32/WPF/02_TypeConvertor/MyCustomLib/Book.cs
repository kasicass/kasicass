using System;

namespace MyCustomLib
{
    public class Book
    {
        public Book() { }
        public string Name { get; set; }
        public MoneyType Price { get; set; }

        public override string ToString()
        {
            return Name + "售价为：" + Price + "元";
        }
    }
}
