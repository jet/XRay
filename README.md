# XRay



XRay is a platform which provides collection, search and analysis of application traces in a distributed system.

The core concepts in XRay are telemetry events, operations and traces.

A telemetry event is a record of an action occurring at a service.

Telemetry events are emitted as part of the execution of operations, also called spans, which are units of work delimited by start and complete events.

Operations are related by propagating a trace id across service boundaries, and the collection of events sharing a trace id form a trace.

In addition to the trace and operation, telemetry events are associated with user defined tags.
XRay indexes telemetry events, allowing lookup by trace id, operation name and tags.

In effect, queries on telemetry events can be used to explore dependencies as well as timing information of distributed traces.
