# DMVCFRAMEWORK COMPATIBILITY MAINTAINERS

dmvcframework-core-team uses always the latest version of Delphi, so testing in previous versions is a problem and some users could have problems with the new DMVCFramework versions. We usually publish a number of RCs but not all the users test them, so when the main version is released some "late adopters" could find anomalies on old Delphi versions.
In the list below there are all the *Compatibility Mantainers* for each specific Delphi versions supported.
The *Compatibility Mantainers* run unit tests for each RC and each finally releases and raise [issues](https://github.com/danieleteti/delphimvcframework/issues) if problems arises during these tests. 

>The issues should be created on github project using the label `COMPATIBILITY MANTAINER`.

If you use a Delphi version that is not the latest, and you find an issue, check here the *Compatibility Mantainers* for your version and create an issue with reference to him (you can also talk about the issue mentioning the mantainer using the facebook group).

## COMPATIBILITY MANTAINERS

|Delphi Version | Compatibility Mantainer |
|---|---|
|Delphi 10.4 Sydney | Daniele Teti |
|Delphi 10.3.1 Rio		|Daniele Teti, João Fernandes, Emilio Perez		|
|Delphi 10.2.3 Tokyo  |Daniele Barbato, Mohammed Nasman, MJ Wille 				|
|Delphi 10.2 Community Edition|Akrom Obidov|
|Delphi 10.1.2 Berlin 	| Emilio Perez, Akrom Obidov, Akbar Khayitov 	|
|Delphi 10.0 Seattle	| Davut Eryılmaz 								|
|                               |                                            |

### What is expected by a compatibility mantainer?

A compatibility maintainer is in charge to test each release candidate and each main version to ensure that the core packages and all the samples works for the Delphi version in charge to him. These are the steps to follow:

### Testing Installation

- Download the new version (or RC) zip from the [releases](https://github.com/danieleteti/delphimvcframework/releases) github page.
- Unzip in a folder which doesn't clash with your existent DMVCFramework installation (double check that the library path doesn't include other versions)
- Build all the packages for your target version (\packages\<yourversion>\dmvcframework_group.groupproj)
- Install `dmvcframeworkDT`.

### Testing IDE Integration

- Close Delphi
- Restart Delphi and check if the DMVCFramework splash appears in the RAD Studio splash screen.
- `File->New->Project` you should see the DMVCFramework Node to create a new project
- Create a new project, leave all the defaults and run it (should run without problems)

### Testing Samples

- Open each samples and test that it compiles ans works as expected
