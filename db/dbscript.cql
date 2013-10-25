create keyspace capickling_tests with replication = { 'class' : 'SimpleStrategy', 'replication_factor' : 1 };

use capickling_tests;

create table users (
		userId int primary key,
		name text
	);

