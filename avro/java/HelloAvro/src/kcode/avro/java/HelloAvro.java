package kcode.avro.java;

import java.io.File;
import java.io.IOException;

import org.apache.avro.Schema;
import org.apache.avro.file.DataFileWriter;
import org.apache.avro.file.DataFileReader;
import org.apache.avro.generic.GenericData;
import org.apache.avro.generic.GenericDatumWriter;
import org.apache.avro.generic.GenericDatumReader;

class HelloAvro {
	static private void appendPerson(DataFileWriter<GenericData.Record> writer,
										Schema aSchema, long id, String name) throws IOException {
		GenericData.Record aPerson = new GenericData.Record(aSchema);
		aPerson.put("ID", id);
		aPerson.put("Name", name);
		writer.append(aPerson);
	}

	static private void writeTest() throws IOException {
		Schema aSchema = Schema.parse(new File("decl/Person.json"));

		DataFileWriter<GenericData.Record> writer = new DataFileWriter<GenericData.Record>(
				new GenericDatumWriter<GenericData.Record>());
		writer.create(aSchema, new File("output/Person.db"));
		appendPerson(writer, aSchema, 1001, "kasicass");
		appendPerson(writer, aSchema, 1002, "phay");
		writer.close();
	}


	static private void printPerson(GenericData.Record aPerson) {
		System.out.println("aPerson, id=" + aPerson.get("ID") + ", name=" + aPerson.get("Name"));
	}

	static private void readTest() throws IOException {
		DataFileReader<GenericData.Record> reader = new DataFileReader<GenericData.Record>(new File("output/Person.db"),
				new GenericDatumReader<GenericData.Record>());
		while (reader.hasNext()) {
			GenericData.Record aPerson = reader.next();
			printPerson(aPerson);
		}
	}

	static public void main(String[] args) throws IOException {
		if (args.length != 1) {
			System.out.println("usage:");
			System.out.println("  ./a.out <readtest|writetest>");
			return;
		}

		if (args[0].equals("readtest")) {
			readTest();
		} else {
			writeTest();
		}
	}
}
