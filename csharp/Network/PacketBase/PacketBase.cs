using System;
using System.IO;
using System.Reflection;

namespace BaddyNet
{
    public class PacketBase
    {
        public void marshal(Stream s)
        {
            BinaryWriter writer = new BinaryWriter(s);
            Type t = this.GetType();
            FieldInfo[] fields = t.GetFields();

            foreach (var fi in fields)
            {
                if (fi.FieldType == typeof(string)) writer.Write((string)fi.GetValue(this));
                else if (fi.FieldType == typeof(int)) writer.Write((int)fi.GetValue(this));
                else if (fi.FieldType == typeof(float)) writer.Write((float)fi.GetValue(this));
                else if (fi.FieldType == typeof(double)) writer.Write((double)fi.GetValue(this));
            }
        }

        public void unmarshal(Stream s)
        {
            BinaryReader reader = new BinaryReader(s);
            Type t = this.GetType();
            FieldInfo[] fields = t.GetFields();

            foreach (var fi in fields)
            {
                if (fi.FieldType == typeof(string)) fi.SetValue(this, reader.ReadString());
                else if (fi.FieldType == typeof(int)) fi.SetValue(this, reader.ReadInt32());
                else if (fi.FieldType == typeof(float)) fi.SetValue(this, reader.ReadSingle());
                else if (fi.FieldType == typeof(double)) fi.SetValue(this, reader.ReadDouble());
            }
        }
    }
}