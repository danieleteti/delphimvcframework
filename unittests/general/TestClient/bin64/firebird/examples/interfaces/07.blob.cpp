/*
 *	PROGRAM:	Object oriented API samples.
 *	MODULE:		07.blob.cpp
 *	DESCRIPTION:	A sample of loading data into blob and reading.
 *					Run second time (when database already exists) to see
 *					how FbException is caught and handled by this code.
 *
 *					Example for the following interfaces:
 *					IAttachment - use of open and create blob methods
 *					IBlob - interface to work with blobs
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
 *  The Original Code was created by Alexander Peshkoff
 *  for the Firebird Open Source RDBMS project.
 *
 *  Copyright (c) 2016 Alexander Peshkoff <peshkoff@mail.ru>
 *  and all contributors signed below.
 *
 *  All Rights Reserved.
 *  Contributor(s): ______________________________________.
 */

#include "ifaceExamples.h"
#include <firebird/Message.h>

static IMaster* master = fb_get_master_interface();

static const char* testData[] = {
	"This is test data",
	"that is inserted during example execution",
	"into blobs_table",
	NULL
};

static void errPrint(IStatus* status)
{
	char buf[256];
	master->getUtilInterface()->formatStatus(buf, sizeof(buf), status);
	fprintf(stderr, "%s\n", buf);
}

static void drop(IAttachment** att)
{
	// With CheckStatusWrapper passed as status interface noting is thrown on error
	// You should check status yourself
	CheckStatusWrapper status(master->getStatus());

	// drop database (will close interface)
	(*att)->dropDatabase(&status);
	if (status.getState() & IStatus::STATE_ERRORS)
	{
		errPrint(&status);
		fprintf(stderr, "*** Drop database failed - do it manually before next run ***\n");
	}
	else
		*att = NULL;

	// cleanup
	status.dispose();
}

int main()
{
	int rc = 0;

	// set default password if none specified in environment
	setenv("ISC_USER", "sysdba", 0);
	setenv("ISC_PASSWORD", "masterkey", 0);

	// With ThrowStatusWrapper passed as status interface FbException will be thrown on error
	ThrowStatusWrapper status(master->getStatus());

	// Declare pointers to required interfaces
	IProvider* prov = master->getDispatcher();
	IAttachment* att = NULL;
	ITransaction* tra = NULL;
	IBlob* blob = NULL;

	try
	{
		// create database
		att = prov->createDatabase(&status, "blob_07.fdb", 0, NULL);
		tra = att->startTransaction(&status, 0, NULL);

		// create table
		att->execute(&status, tra, 0, "create table blobs_table (b blob sub_type text)", SAMPLES_DIALECT,
			NULL, NULL, NULL, NULL);
		tra->commitRetaining(&status);

		// Message for data exchange
		FB_MESSAGE(Msg, ThrowStatusWrapper,
			(FB_BLOB, b)
		) message(&status, master);
		message.clear();

		// create blob
		blob = att->createBlob(&status, tra, &message->b, 0, NULL);

		// populate blob with data
		for (const char** seg = testData; *seg; ++seg)
			blob->putSegment(&status, strlen(*seg), *seg);
		blob->close(&status);
		blob = NULL;

		// insert blob into the table
		att->execute(&status, tra, 0, "insert into blobs_table(b) values(?)", SAMPLES_DIALECT,
			message.getMetadata(), message.getData(), NULL, NULL);
		tra->commitRetaining(&status);
		printf("Test blob inserted into blobs_table\n...\n");

		// Read blob from table
		message.clear();
		att->execute(&status, tra, 0, "select first(1) b from blobs_table", SAMPLES_DIALECT,
			NULL, NULL, message.getMetadata(), message.getData());
		blob = att->openBlob(&status, tra, &message->b, 0, NULL);

		// Read segments from blob
		// Use very small segment buffer to show read of incomplete segment
		printf("Read inserted blob from blobs_table\n...\n");
		int bufOver = 0;
		for(bool eof = false; !eof; )
		{
			const char* lineFeed = "\n";
			char buf[32];
			unsigned l = 0;
			switch (blob->getSegment(&status, sizeof(buf) - 1, buf, &l))
			{
				case IStatus::RESULT_OK:
					break;
				case IStatus::RESULT_SEGMENT:
					lineFeed = "";
					bufOver++;
					break;
				default:
					eof = true;
					continue;
			}
			buf[l] = 0;
			printf("%s%s", buf, lineFeed);
		}
		printf("\nSegment not fit in buffer counter = %d\n\n", bufOver);

		// cleanup
		blob->close(&status);
		blob = NULL;
		tra->commit(&status);
		tra = NULL;

		// uncomment next line to play with errors during drop database
		// printf("Attach with any client to blob_07.fdb to prevent it being dropped and press enter"); getchar();

		// drop database
		drop(&att);
	}
	catch (const FbException& error)
	{
		// handle error
		rc = 1;
		errPrint(error.getStatus());

		if (att)
			drop(&att);
	}

	// release interfaces after error caught
	if (blob)
		blob->release();
	if (tra)
		tra->release();
	if (att)
		att->release();

	status.dispose();
	prov->release();

	return rc;
}
