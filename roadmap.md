# DelphiMVCFramework - ROADMAP
The DelphiMVCFramework team is working hard to bring to the Open Source Delphi community an even better framework for the Q4 2019 and Q1 2020. 
In this document are defined all our plans for the next features. As you know DMVCFramework is used in many big and small companies  all around the world and many of these companies ask for specific features, so this roadmap tries to focus on these requests. 
As we are in the detailed planning stages for these features, we will share additional details as we get further into 2020. If you have specific items or questions, please let the development team know and we can talk about it (email: dmvcframework@bittime.it). Also if we are working harder to bring these functionalities to the light, there is no certainty that they will be implemented in the time or in the way we planned in this document. If you need some specific functionality according to your needs and in your time, request a quote for custom development (email: dmvcframework@bittime.it).

 - (IN PROGRESS) Publish the [DelphiMVCFramework Handbook](https://leanpub.com/delphimvcframework)
 - (DONE) Switch from the internal logger to the [LoggerPro](https://github.com/danieleteti/loggerpro) project to have more flexibility
 - (DONE) Remove eLua support and switch to [Mustache](https://github.com/synopse/dmustache) for server side view
 - (DONE) Implement JWT support
 - (DONE) Implement Strongly Typed Actions
 - (DONE) Implement Custom Authentication and Authorization Middleware
 - (DONE) Use a middleware to implement response compression for console type projects
 - (DONE)Implement Swagger support 
 - (DONE) Linux support
 - (DONE) Update Mapper framework to make it extensible and configurable
   - The default mapper interface will be the same as the current version so that no breaking changes happened
   - Will be possible to register a specific serializer/deserializer engine for each type or property name
 - (DONE) Switch from the System.JSON parser to the [JsonDataObjects](https://github.com/ahausladen/JsonDataObjects) parser (which is more than 10 times faster).
   - This point has the max priority!
 - (DONE, BUT WE NEED MORE) Improve the IDE expert
 - Add the XML support for the Mapper using the [OmniXML](https://github.com/mremec/omnixml) project (low priority: looking for volunteers)
 - (DONE) Create a mechanism to allows a fast CRUD interface based on tables
   - With this features you can simply define the resource name, the database table name which that resource points to and the allowed standard CRUD actions, and you have a complete CRUD system. Will be a lot useful for all that simple entities which don't have complex business logic.
   - Will be based on FireDAC
- New samples with specific web related use cases:
   - WebWorkers
   - (DONE) Angular2+
   - (DONE) React
- (DONE) Create "Custom Authentication and Authorization" demo
- (CANCELED) Complete the [DevGuide](https://danieleteti.gitbooks.io/delphimvcframework/content/) on gitbooks
- Improve the session mechanism to allows more flexibility
