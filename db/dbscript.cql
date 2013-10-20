create keyspace capickle_tests with replication = { 'class' : 'SimpleStrategy', 'replication_factor' : 1 };

use capickle_tests;

create table users (
		userId int primary key,
		name text
	);

