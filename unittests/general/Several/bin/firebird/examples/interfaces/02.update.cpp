/*
 *	PROGRAM:	Object oriented API samples.
 *	MODULE:		02.update.cpp
 *	DESCRIPTION:	Run once prepared statement with parameters
 *					a few times, committing transaction after each run.
 *					Learns how to prepare statement, manually define parameters
 *					for it, execute that statement with different parameters
 *					and perform non-default error processing.
 *
 *					Example for the following interfaces:
 *					IAttachment - database attachment
 *					ITransaction - transaction
 *					IStatement - SQL statement execution
 *					IMessageMetadata - describe input and output data format
 *					IMetadataBuilder - tool to modify/create metadata
 *					IStatus - return state holder
 *
 *					Note that all updates are rolled back in this version. (see *** later)
 *
 * The contents of this file are subject to the Interbase Public
 * License Version 1.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy
 * of the License at http://www.Inprise.com/IPL.html
 *
 * Software distributed under the License is distributed on an
 * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
 * or implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code was created by Inprise Corporation
 * and its predecessors. Portions created by Inprise Corporation are
 * Copyright (C) Inprise Corporation.
 *
 * All Rights Reserved.
 * Contributor(s): ______________________________________.
 *					Alex Peshkov, 2013
 */

#include "ifaceExamples.h"

static IMaster* master = fb_get_master_interface();

int get_input(char*, double*);
static const char* Dept_data[] =
     {"622", "100", "116", "900", 0};
static double Percent_data[] =
    {0.05,  1.00,  0.075,  0.10, 0};
int Input_ptr = 0;

int main()
{
	int rc = 0;

	// set default password if none specified in environment
	setenv("ISC_USER", "sysdba", 0);
	setenv("ISC_PASSWORD", "masterkey", 0);

	// status vector and main dispatcher
	ThrowStatusWrapper status(master->getStatus());
	IProvider* prov = master->getDispatcher();

	// declare pointers to required interfaces
	IAttachment* att = NULL;
	ITransaction* tra = NULL;

	// Interface executes prepared SQL statement
	IStatement* stmt = NULL;

	// Interfaces provides access to format of data in messages
	IMessageMetadata* meta = NULL;

	// Interface makes it possible to change format of data or define it yourself
	IMetadataBuilder* builder = NULL;

	const char* updstr =
	    "UPDATE department SET budget = ? * budget + budget WHERE dept_no = ?";

	try
	{
		// attach employee db
		att = prov->attachDatabase(&status, "employee", 0, NULL);

		// start transaction
		tra = att->startTransaction(&status, 0, NULL);

		// prepare statement
		stmt = att->prepare(&status, tra, 0, updstr, SAMPLES_DIALECT, 0);

		// build metadata
		// IMaster creates empty new metadata in builder
		builder = master->getMetadataBuilder(&status, 2);
		// set required info on fields
		builder->setType(&status, 0, SQL_DOUBLE + 1);
		builder->setType(&status, 1, SQL_TEXT + 1);
		builder->setLength(&status, 1, 3);
		// IMetadata should be ready
		meta = builder->getMetadata(&status);
		// no need in builder any more
		builder->release();
		builder = NULL;

		// allocate buffer on stack
		char buffer[256];
		unsigned len = meta->getMessageLength(&status);
		if (len > sizeof(buffer))
		{
			throw "Input message length too big - can't continue";
		}

		// locations of parameters in input message
		char* dept_no = &buffer[meta->getOffset(&status, 1)];
 		double* percent_inc = (double*) &buffer[meta->getOffset(&status, 0)];

		// null IDs (set to NOT NULL)
 		short* flag = (short*)&buffer[meta->getNullOffset(&status, 0)];
 		*flag = 0;
 		flag = (short*) &buffer[meta->getNullOffset(&status, 1)];
 		*flag = 0;

		// Get the next department-percent increase input pair.
	    while (get_input(dept_no, percent_inc))
    	{
	        printf("\nIncreasing budget for department:  %s  by %5.2lf percent.\n",
    	           dept_no, *percent_inc);

			// Update the budget.
			try
			{
			    stmt->execute(&status, tra, meta, buffer, NULL, NULL);
			}
			catch (const FbException& error)
			{
				// Handle exception raised during statement execution
				if (error.getStatus()->getErrors()[1] == isc_not_valid)
				{
					// Don't save the update, if the new budget exceeds the limit.
					printf("\tExceeded budget limit -- not updated.\n");

					tra->rollbackRetaining(&status);
					continue;
				}

				// Another error - use default handler
				throw;
			}

			// Save each department's update independently.
			// *** Change to commitRetaining() to see changes
			// *** tra->commitRetaining(&status);
			tra->rollbackRetaining(&status);
        }

		// close interfaces
		stmt->free(&status);
		stmt = NULL;

		meta->release();
		meta = NULL;

		tra->commit(&status);
		tra = NULL;

		att->detach(&status);
		att = NULL;
	}
	catch (const FbException& error)
	{
		// handle error
		rc = 1;

		char buf[256];
		master->getUtilInterface()->formatStatus(buf, sizeof(buf), error.getStatus());
		fprintf(stderr, "%s\n", buf);
	}

	// release interfaces after error caught
	if (builder)
		builder->release();
	if (meta)
		meta->release();
	if (stmt)
		stmt->release();
	if (tra)
		tra->release();
	if (att)
		att->release();

	prov->release();
	status.dispose();

	return rc;
}


/*
 *  Get the department and percent parameters for an example to run.
 */

int get_input (char *dept_no, double *percent)
{
    if (Dept_data[Input_ptr] == 0)
        return 0;

    strcpy(dept_no, Dept_data[Input_ptr]);

    if ((*percent = Percent_data[Input_ptr++]) == 0)
        return 0;

    return 1;
}
