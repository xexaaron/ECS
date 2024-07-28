# README.md

## Disclaimer
   * #### This probably should <u>not</u> be used in production code. 
   * #### Please use [entt](https://github.com/skypjack/entt) instead of this.

## Building
   * #### <u>C++ 20</u> or higher required

   * #### <u>Single header file</u>.  Just drop it into your include directory and include it.


### Compilation Flags & Values
   * #### Compilation flags can be found at the top of [ECS.h](Source/ECS.h#L17).

   * ```cpp    
      #undef ECS_USE_64_BIT_HANDLES       // Use 64 bit identifiers for entities instead of 32 bit.
      ```     
   * ```cpp    
      #undef ECS_NO_LOG                   // Turn off logging for the ecs entirely (including errors).
      ```   
   * ```cpp    
      #undef ECS_NO_ASSERT                // Turn off asserts in debug builds. (in release it is off by default).
      ```   
   * ```cpp    
      #undef ECS_USE_FN_COMP_DESTRUCTION  // Destroy components using a stack of functions rather than a vector unique_ptr registries.
      ```      
   * ```cpp
      #define ECS_MAX_COMPONENT_TYPES 128 // The maximum number of unique component types. (this will determine the size of the bitset used for checking if an entity has a component).
      ```
      
## Common language

### A uint32_t or uint64_t value depending on [compilation flags](Source/ECS.h#L31).

 * ```cpp    
    typename Abyss::Handle 
    ```
### A component type

 * ```cpp    
    template<typename C> 
    ```
### A list of component types.

 * ```cpp    
    template<typename ...Cs>
    ``` 



## Examples

### Basic
   ```cpp
      #include "ECS.h"
      
      int main() {
         using namespace Abyss;

         struct MyComponent {
            float foo = 0.5f;
            double bar = 5.5; 
            int xan = 10; 
         };
         
         struct MyOtherComponent {
            float foo = 0.5f;
            char c = '/';
         }

         // Create an entity      
         for (size_t i = 0; i < 100; ++i) {
            Entity entity = ECS::Create();
            if (i % 2 == 0) {
               // Add a component to it. Only one Component per Component Type allowed.
               entity.AddComponent<MyComponent>();
               auto& my_comp = entity.GetComponent<MyComponent>();
               my_comp.foo = 10.f;
               
               entity.AddComponent<int>();
               auto& integer = entity.GetComponent<int>();
               integer = 5;
            } else {
               entity.AddComponent<MyOtherComponent>();
               auto& my_comp = entity.GetComponent<MyOtherComponent>();
               my_comp.c = 'b';
            }
         }
         View<Entity> view   = ECS::View<MyComponent>();      // View entities that have MyComponent
         View<Entity> view_2 = ECS::View<MyComponent, int>(); // View entities that have MyComponent AND int components.

         View<Entity> other_view = ECS::View<MyOtherComponent>(); // view entities that have MyOtherComponent
         View<Entity> group = ECS::Group<MyComponent, MyOtherComponent>(); // group entites that have either MyComponent OR MyOtherComponent.

         for (auto entity : ECS()) {}       // Loop over the ECS using begin and end. Perferred over ECS::View().
         for (auto entity : ECS::View()) {} // Loop over a view of the entire ECS 

         ECS::Clear(); // Clear the ecs if desired.
      }

   ```

### [Logging](./Source/ECS.h#L156) 
   * #### Mostly for debug purposed as asserts are used in place of errors.
   * #### Here is the [default Logger](./Source/ECS.h#L181) implementation used by the ECS.

   ```cpp
      class DefaultLogger final : public ILogger {
      public:
         void LogError(const std::string &msg) const override {
            constexpr auto RESET = "\033[0m";
            constexpr auto RED = "\033[31m";
            std::cerr << RED << "[ECS] [ERROR] : " << msg << RESET << std::endl;
         }
         void LogDebug(const std::string &msg) const override {
            constexpr auto RESET = "\033[0m";
            constexpr auto DARK_GREY = "\033[38;5;238m";
            std::cout << DARK_GREY << "[ECS] [DEBUG] : " << msg << RESET << std::endl;
         };

      private:
      };
   ```

   * #### You can set the logger using two methods.

   ```cpp
      ILogger::SetLogger(std::make_unique<MyLogger>());
   ```
   ```cpp
      ECS::SetLogger(std::make_unique<MyLogger>());
   ```


