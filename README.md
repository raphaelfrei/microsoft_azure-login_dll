# DLL - Login into Microsoft Azure

Library that can be used to integrated personal system into Microsoft Azure's Services.

This script works with any software that integrates an DLL.
(I'm currently using with OpenEdge ABL Progress)



	- How To Use:

	The AzureLogin function requires 3 entry arguments: "User ID", "Password" and "Domain".

	It returns an STRING value:

	When the login success, it returns "SUCCESS - User 'User ID' has logged in."

	When it fails, it returns "ERROR - While logging in: 'Error'" - Saying what the issue is (Can be wrong password, no internet connection, account locked, ...)

.

	- Example (with OpenEdge):

	DEF VAR lSuccess AS CHAR NO-UNDO.
	lSuccess = RF.Control:AzureLogin(txtUser:SCREEN-VALUE, txtPassword:SCREEN-VALUE, "MYDOMAIN.com").

(It requires System.DirectoryServices to work)
