### trac-to-phabricator

##### To do

 - [ ] Extract Trac Postrgres connection details to configuration
 - [ ] Extract Phabricator MySQL connection details to configuration
 - [ ] Extract GHC project PHID to configuration
 - [ ] Extract Phabricator API details to configuration
 - [ ] Copy comment history across to Phabricator
 - [ ] Copy custom field data across to Phabricator
 - [ ] Copy attachments across to Phabricator
 - [ ] Alert when target user does not exist

##### Usage

Currently all configuration is hard coded, so you'll need to change those parts - see the to do list for the list of things that would need to be changed.

The custom field and status definitions for Maniphest must be uploaded to Phabricator before importing. To do this, copy the contents of each file in `maniphest-config/` to the configuration of the same name in the Phabricator settings.

# Migrating Users

There are some users who have trac accounts who do not have phabricator accounts.
It is possible to create accounts for these users using the `accountadmin` utility
with empty passwords so that if one of these users wishes to modify their
tickets after the migration they can recover their account using the registered email
address and act as normal.
