create table fba.roles(
   "roleId" text, 
   "code" text, 
   "name" text
);
insert into roles("roleId","code","name") values(gen_random_uuid(),'manager','Manager');