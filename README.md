NOTICE: SUPPORT FOR THIS PROJECT ENDED ON 18 November 2020

This projected was owned and maintained by Jet.com (Walmart). This project has reached its end of life and Walmart no longer supports this project.

We will no longer be monitoring the issues for this project or reviewing pull requests. You are free to continue using this project under the license terms or forks of this project at your own risk. This project is no longer subject to Jet.com/Walmart's bug bounty program or other security monitoring.


## Actions you can take

We recommend you take the following action:

  * Review any configuration files used for build automation and make appropriate updates to remove or replace this project
  * Notify other members of your team and/or organization of this change
  * Notify your security team to help you evaluate alternative options

## Forking and transition of ownership

For [security reasons](https://www.theregister.co.uk/2018/11/26/npm_repo_bitcoin_stealer/), Walmart does not transfer the ownership of our primary repos on Github or other platforms to other individuals/organizations. Further, we do not transfer ownership of packages for public package management systems.

If you would like to fork this package and continue development, you should choose a new name for the project and create your own packages, build automation, etc.

Please review the licensing terms of this project, which continue to be in effect even after decommission.

ORIGINAL README BELOW

----------------------



# XRay



XRay is a platform which provides collection, search and analysis of application traces in a distributed system.

The core concepts in XRay are telemetry events, operations and traces.

A telemetry event is a record of an action occurring at a service.

Telemetry events are emitted as part of the execution of operations, also called spans, which are units of work delimited by start and complete events.

Operations are related by propagating a trace id across service boundaries, and the collection of events sharing a trace id form a trace.

In addition to the trace and operation, telemetry events are associated with user defined tags.
XRay indexes telemetry events, allowing lookup by trace id, operation name, and tags.

In effect, queries on telemetry events can be used to explore dependencies as well as timing information of distributed traces.
