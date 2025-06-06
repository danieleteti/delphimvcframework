/*
 *	PROGRAM:		Firebird users migration.
 *	MODULE:			security_database.sql
 *	DESCRIPTION:	Migrate users from fb2.x format into fb3
 *					with random passwords.
 *
 *  The contents of this file are subject to the Initial
 *  Developer's Public License Version 1.0 (the "License");
 *  you may not use this file except in compliance with the
 *  License. You may obtain a copy of the License at
 *  http://www.ibphoenix.com/main.nfs?a=ibphoenix&page=ibp_idpl.
 *
 *  Software distributed under the License is distributed AS IS,
 *  WITHOUT WARRANTY OF ANY KIND, either express or implied.
 *  See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Original Code was created by Alex Peshkov
 *  for the Firebird Open Source RDBMS project.
 *
 *  Copyright (c) 2016 Alex Peshkov <peshkoff at mail.ru>
 *  and all contributors signed below.
 *
 *  All Rights Reserved.
 *  Contributor(s): ______________________________________.
 *
 *
 */

set term ^;

execute block returns(usr varchar(31), passwd varchar(36))
as
declare variable frst varchar(32);
declare variable mddl varchar(32);
declare variable lst varchar(32);
declare variable attr varchar(4096);
declare variable sql varchar(4096);
declare variable uid int;
declare variable gid int;

begin
for select rdb$user_name, rdb$first_name, rdb$middle_name, rdb$last_name, rdb$uid, rdb$gid,
	uuid_to_char(gen_uuid()) from rdb$users
	where rdb$user_name is not null and upper(rdb$user_name) != 'SYSDBA'
into :usr, :frst, :mddl, :lst, :uid, :gid, :passwd
do begin
	-- basic fields
	sql = 'create or alter user "' || usr || '" password ''' || passwd || '''';
	if (frst is not null) then sql = sql || ' firstname ''' || frst || '''';
	if (mddl is not null) then sql = sql || ' middlename ''' || mddl || '''';
	if (lst is not null) then sql = sql || ' lastname ''' || lst || '''';
	sql = sql || ' active';

	-- attributes
	attr = '';
	if (uid is not null) then attr = 'uid=''' || uid || '''';
	if (gid is not null) then begin
		if (char_length(attr) > 0) then attr = attr || ', ';
		attr = attr || 'gid=''' || gid || '''';
	end
	if (char_length(attr) > 0) then begin
		sql = sql || ' tags (' || attr || ')';
	end

	-- create it
	execute statement sql;
	-- and show password to admin
	suspend;
end
end^

commit^

exit^
