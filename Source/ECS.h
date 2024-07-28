#pragma once

#include <unordered_map>
#include <bitset>
#include <span>
#include <type_traits>
#include <numeric>
#include <string_view>
#include <algorithm>
#include <random>
#include <iostream>
#include <sstream>
#include <memory>

// FLAGS
#undef  ECS_USE_64_BIT_HANDLES      // Use 64 bit identifiers for entities instead of 32 bit.
#undef  ECS_NO_LOG                  // Turn off logging for the ecs entirely (including errors).
#undef  ECS_NO_ASSERT               // Turn off asserts in debug builds. (in release it is off by default).
#undef  ECS_USE_FN_COMP_DESTRUCTION // Destroy components using a stack of functions rather than unique_ptrs.

#define ECS_MAX_COMPONENT_TYPES 128 // The maximum number of unique component types.

#pragma region Macros
#ifdef ECS_USE_64_BIT_HANDLES
	namespace Abyss { 
		using Handle = std::uint64_t;
	}
#else
	namespace Abyss { 
		using Handle = std::uint32_t;
	}
#endif

#ifdef ECS_USE_FN_COMP_DESTRUCTION
	#include <functional>
	#include <stack>

    #define COMPONENT_REMOVER_CLASS ComponentRemover
    #define COMPONENT_REMOVER_PUSH_ARGS const std::function<void()>& fn
    #define COMPONENT_REMOVER_VARS std::stack<std::function<void()>> m_CleanupFns
    #define COMPONENT_REGISTRY_CTOR_ACCESS private
    #define COMPONENT_REGISTRY_VIRTUAL_IMPL(suffix)
    #define COMPONENT_REGISTRY_INHERITANCE final
#else
    #define COMPONENT_REMOVER_CLASS ComponentRemoverEx
    #define COMPONENT_REMOVER_PUSH_ARGS std::unique_ptr<ComponentRegistryBase> registry
    #define COMPONENT_REMOVER_VARS std::vector<std::unique_ptr<ComponentRegistryBase>> m_Registries;
    #define COMPONENT_REGISTRY_CTOR_ACCESS public
    #define COMPONENT_REGISTRY_VIRTUAL_IMPL(suffix) virtual void Destroy(Handle handle) suffix;  virtual std::string_view Name() suffix;
    #define COMPONENT_REGISTRY_INHERITANCE : public ComponentRegistryBase
    namespace Abyss { 
        class ComponentRegistryBase {
        public:
			#ifdef __clang__
				virtual ~ComponentRegistryBase() = default;
			#endif
		private:

            friend class ComponentRemoverEx;
            COMPONENT_REGISTRY_VIRTUAL_IMPL(= 0);
        };
    }
#endif
#pragma endregion

// Forward Declarations
namespace Abyss {
	template <typename> class View; 
	template <typename> class Type; 
}

// concepts 
namespace Abyss {
    
    template <std::size_t Index, typename T>
    struct param_type;

    template <std::size_t Index, typename Ret, typename... Args>
    struct param_type<Index, Ret(Args...)> {
        using type = typename std::tuple_element<Index, std::tuple<Args...>>::type;
    };

    // Convenience alias
    template <std::size_t Index, typename T>
    using param_type_t = typename param_type<Index, T>::type;

    // Trait to check if the parameter type matches the given type
    template <std::size_t Index, typename Fn, typename T>
    struct param_is : std::is_same<param_type_t<Index, Fn>, T> {};

    // Convenience alias
    template <std::size_t Index, typename Fn, typename T>
    inline constexpr bool param_is_v = param_is<Index, Fn, T>::value;
    
    /**
    * @brief Concept to check if a fn parameter is of a given type.
    * 
    * @tparam Index (0 based).
    * @tparam Fn Function type.
    * @tparam T  Type to check for.
    */
    template <std::size_t Index, typename Fn, typename T>
    concept CFnParamIsOfType = (std::is_invocable_v<Fn, T> || param_is_v<Index, Fn, T>);

    template <std::size_t N, typename... Args>
    concept CArgCountGreater = sizeof...(Args) > N;

    template <std::size_t N, typename... Args>
    concept CArgCountLess = sizeof...(Args) < N;

    template <std::size_t N, typename... Args>
    concept CArgCountGreaterEqual = sizeof...(Args) >= N;

    template <std::size_t N, typename... Args>
    concept CArgCountLessEqual = sizeof...(Args) <= N;

    template <std::size_t N, typename... Args>
    concept CArgCountEqual= sizeof...(Args) == N;
   
    template <typename T, typename Char>
    concept CIsCharArray = (std::is_array_v<T> && std::is_same_v<std::remove_extent_t<T>, Char>);

    template <typename T>
    concept CIsString =        
        (std::is_same_v<T, std::string>     ||
         std::is_same_v<T, std::u8string>   ||
         std::is_same_v<T, std::u16string>  ||
         std::is_same_v<T, std::u32string>  ||
         std::is_same_v<T, std::wstring>    ||
         std::is_same_v<T, const char*>     ||
         std::is_same_v<T, char*>           ||
         std::is_same_v<T, const char8_t*>  ||
         std::is_same_v<T, char8_t*>        ||
         std::is_same_v<T, const char16_t*> ||
         std::is_same_v<T, char16_t*>       ||
         std::is_same_v<T, const char32_t*> ||
         std::is_same_v<T, char32_t*>       ||
         std::is_same_v<T, const wchar_t*>  || 
         std::is_same_v<T, wchar_t*>        ||
         CIsCharArray<T, char>              ||
         CIsCharArray<T, char8_t>           ||
         CIsCharArray<T, char16_t>          ||
         CIsCharArray<T, char32_t>          ||
         CIsCharArray<T, wchar_t>);
     
}

#pragma region Logger
namespace Abyss {
	/**
	 * @brief The logger interface used by the ECS.
	 * @param virtual Override the virtual methods and call ILogger::SetLogger() to use custom loggers.
	 * @param msg Formatted log message.
	 */
	class ILogger {
	public:
		ILogger() {
    		std::cout.setf(std::ios_base::boolalpha);
    		std::cin.setf(std::ios_base::boolalpha);
		}

		#ifdef __clang__
			virtual ~ILogger() = default;
		#endif	

		inline static void SetLogger(std::unique_ptr<ILogger> logger = nullptr) {
			s_Logger = std::move(logger);
		}
		inline static const std::unique_ptr<ILogger>& GetLogger() {
			return s_Logger;
		}

		virtual void LogError(const std::string &msg) const = 0;
		virtual void LogDebug(const std::string &msg) const = 0;

	private:
		static std::unique_ptr<ILogger> s_Logger;
	};

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

	std::unique_ptr<ILogger> ILogger::s_Logger = std::make_unique<DefaultLogger>();
}

#ifdef __GNUC__
	#define ECS_FUNC_SIG __PRETTY_FUNCTION__
#elif defined(_MSC_VER)
	#define ECS_FUNC_SIG __FUNCSIG__
#else // Unknown
	#if defined(__INTEL_COMPILER) && (__INTEL_COMPILER >= 600)) || (defined(__IBMCPP__) && (__IBMCPP__ >= 500)
		#define ECS_FUNC_SIG __FUNCTION__
	#elif defined(__BORLANDC__) && (__BORLANDC__ >= 0x550)
		#define ECS_FUNC_SIG __FUNC__
	#else
		#define ECS_FUNC_SIG __func__
	#endif // __FUNCTION__
#endif // __GNUC__

#ifdef NDEBUG
	#define ECS_LOG_DEBUG(...)
	#define ECS_ASSERT(...)
#else
	#define ECS_LOG_DEBUG(...)                                               \
		if (auto& logger = ILogger::GetLogger())                              \
		{                                                                    \
			auto tup = std::make_tuple(__VA_ARGS__);                         \
			std::stringstream ss;                                            \
			std::apply([&ss](auto &&...args) { ((ss << args), ...); }, tup); \
			logger->LogDebug(ss.str());                        				 \
		}
	#define ECS_ASSERT(statement, ...) do { if (!statement) { ECS_LOG_ERROR(__VA_ARGS__); __debugbreak(); } } while(0)
#endif

#ifdef ECS_NO_ASSERT
	#undef ESC_ASSERT
	#define ECS_ASSERT(...)
#endif

#ifdef ECS_NO_LOG
	#undef ECS_LOG_DEBUG
	#undef ECS_LOG_ERROR
	#define ECS_LOG_DEBUG(...)
	#define ECS_LOG_ERROR(...)
#else
	#define ECS_LOG_ERROR(...)                                               \
		if (auto& logger = ILogger::GetLogger())                              \
		{                                                                    \
			auto tup = std::make_tuple(__VA_ARGS__);                         \
			std::stringstream ss;                                            \
			ss << "[" << ECS_FUNC_SIG << "] -> ";                            \
			std::apply([&ss](auto &&...args) { ((ss << args), ...); }, tup); \
			logger->LogError(ss.str());                        				 \
		}
#endif
#pragma endregion

// Public Interface
namespace Abyss {

   	class Entity {
	public:
		Entity(); // Default Null Constructor
		Entity(const Entity& other);  
		Entity(Abyss::Handle handle); 
	public:
		void Destroy();

		template <typename C> 
		inline void AddComponent(const C& component) requires (std::is_copy_constructible_v<C>);

		template <typename C> 
		inline void AddComponent() requires (std::is_default_constructible_v<C>);

		template <typename C, typename ...Args> 
		inline void AddComponent(Args... args) requires (std::is_constructible_v<C, Args...>);

		template <typename C>
		inline void RemoveComponent();

		template <typename C>
		inline const C& GetComponent() const;

		template <typename C>
		inline C& GetComponent();

		template <typename ...Cs>
		inline auto GetComponents() -> std::tuple<Cs&...>;
		
		template <typename ...Cs>
		inline auto GetComponents() const -> const std::tuple<Cs&...>;

		template <typename C>
		inline bool HasComponent() const;

		template <typename ...Cs>
		inline bool HasComponents() const;

		inline size_t Components() const;
	public:
 		explicit operator Abyss::Handle() const { return this->m_Handle; }
        explicit operator bool() const;
        bool operator<(Entity other)  const { return this->m_Handle <  other.m_Handle; }
        bool operator>(Entity other)  const { return this->m_Handle >  other.m_Handle; }
        bool operator<=(Entity other) const { return this->m_Handle <= other.m_Handle; }
        bool operator>=(Entity other) const { return this->m_Handle >= other.m_Handle; }
        bool operator==(Entity other) const { return this->m_Handle == other.m_Handle; }
		inline friend std::ostream& operator<<(std::ostream& os, Entity entity) { return os << entity.m_Handle; }
	private:
		Abyss::Handle m_Handle;
		friend class ECS;
	};

	class ECS {
	public:	
		/**
		 * @brief Create a new entity.
		 * @return Entity 
		 */
		static Entity Create();
		/**
		 * @brief Remove an entity and its components from the ecs.
		 * @return bool indicating success.
		 */
		static bool Destroy(Entity entity); 
		/**
		 * @brief Remove all entities and their components from the ecs.
		 */
		static void Clear();
		/**
		 * @brief The amount of entities in the ecs.
		 * @return size_t 
		 */
		static size_t Size();
		/**
		 * @brief Set the Logger for custom log handling, see ECS/Utility/Logger.h for implementing the interface. 
		 * @param logger derived ILogger instance.
		 */
		static void SetLogger(std::unique_ptr<ILogger> logger = std::make_unique<DefaultLogger>()); 
		
		/**
		 * @brief View all entities.
		 * @return std::vector<Entity>
		 */
		static Abyss::View<Entity> View();
		/**
		 * @brief View a range of entities given that end doesnt exceed Size() - 1.
		 * @param begin range start. 
		 * @param end range end.
		 * @return std::vector<Entity>
		 */
		static Abyss::View<Entity> View(size_t begin, size_t end = Size() - 1);
		/**
		 * @brief View all entities with a given component type.
		 * @tparam C component type.
		 * @return std::vector<Entity>
		 */
		template <typename C>
		static Abyss::View<Entity> View();
		/**
		 * @brief View all entities that contain all given component types.
		 * @tparam Cs component types.
		 * @return std::vector<Entity>
		 */
		template <typename ...Cs> requires (CArgCountGreater<1, Cs...>)
		static Abyss::View<Entity> View();
		/**
		 * @brief Group all entites that have atleast one specificed component type.
		 * @tparam Cs component types.
		 * @return std::vector<Entity>
		 */
		template <typename ...Cs> requires (CArgCountGreater<0, Cs...>)
		static Abyss::View<Entity> Group();
		/**
		 * @brief Iterate all entities applying a given function to each.
		 * @tparam Fn @code void(*)(Entity) @endcode (auto deduced).
		 */
		template <typename Fn = void(*)(Entity)> requires (CFnParamIsOfType<0, Fn, Entity>)
		static void Iterate(Fn&& fn);
	public:
		enum class EStatistics {
			ENTITIES,
			REGISTRIES,
			COMPONENTS,
		};
		struct SStat {
			std::string Name = "";
			size_t Value = 0;
		};
		static std::unordered_map<EStatistics, SStat> Statistics();
	private:
		using HandleIterator = std::vector<Handle>::iterator;           
		using ConstHandleIterator = std::vector<Handle>::const_iterator;
		class iterator {                                                               
		public:                                                         
			using iterator_category = std::forward_iterator_tag;        
			using value_type = Entity;                                  
			using difference_type = std::ptrdiff_t;                     
			using pointer = Entity *;                                   
			using reference = Entity &;                                 
																		
			iterator(HandleIterator it)                                 
				: m_It(it)                                              
			{                                                           
			}                                                           
																		
			value_type operator*() const                                
			{                                                           
				return Entity(*m_It);                                   
			}                                                           
																		
			iterator &operator++()                                      
			{                                                           
				++m_It;                                                 
				return *this;                                           
			}                                                           
																		
			iterator operator++(int)                                    
			{                                                           
				iterator tmp = *this;                                   
				++(*this);                                              
				return tmp;                                             
			}                                                           
																		
			bool operator==(const iterator &other) const                
			{                                                           
				return m_It == other.m_It;                              
			}                                                           
																		
			bool operator!=(const iterator &other) const                
			{                                                           
				return !(*this == other);                               
			}                                                           
																		
		private:                                                        
			HandleIterator m_It;                                        
		};                                                              
		class const_iterator {                                                               
		public:                                                         
			using iterator_category = std::forward_iterator_tag;        
			using value_type = Entity;                                  
			using difference_type = std::ptrdiff_t;                     
			using pointer = const Entity *;                             
			using reference = const Entity &;                           
																		
			const_iterator(ConstHandleIterator it)                      
				: m_It(it)                                              
			{                                                           
			}                                                           
																		
			value_type operator*() const                                
			{                                                           
				return Entity(*m_It);                                   
			}                                                           
																		
			const_iterator &operator++()                                
			{                                                           
				++m_It;                                                 
				return *this;                                           
			}                                                           
																		
			const_iterator operator++(int)                              
			{                                                           
				const_iterator tmp = *this;                             
				++(*this);                                              
				return tmp;                                             
			}                                                           
																		
			bool operator==(const const_iterator &other) const          
			{                                                           
				return m_It == other.m_It;                              
			}                                                           
																		
			bool operator!=(const const_iterator &other) const          
			{                                                           
				return !(*this == other);                               
			}                                                           
																		
		private:                                                        
			ConstHandleIterator m_It;                                   
		};
	public:
		iterator begin() noexcept; 
		iterator end() noexcept; 
		const_iterator begin() const noexcept; 
		const_iterator end() const noexcept;
	private:
		template <typename C>
		inline static void AddComponent(Entity entity, const C& component) requires (std::is_copy_constructible_v<C>);

		template <typename C> 
		inline static void AddComponent(Entity entity) requires (std::is_default_constructible_v<C>);

		template <typename C, typename ...Args>
		inline static void AddComponent(Entity entity, Args... args) requires (std::is_constructible_v<C, Args...>);
		
		template <typename C>
		inline static void RemoveComponent(Entity entity);
	
		template <typename C>
		inline static C& GetComponent(Entity entity);

		template <typename ...Cs>
		inline static auto GetComponents(Entity entity) -> std::tuple<Cs&...>;

		template <typename C>
		inline static bool HasComponent(Entity entity);

		template <typename ...Cs>
		inline static bool HasComponents(Entity entity);

		inline static size_t Components(Entity entity);

		template <typename C>
		inline static bool ComponentExists(Entity entity);
	private:
		static std::vector<Handle> s_Handles;
		friend class Entity;
	};
	
    class COMPONENT_REMOVER_CLASS {
    public:
        void Push(COMPONENT_REMOVER_PUSH_ARGS);
        void Flush(Handle handle);
    private:
        COMPONENT_REMOVER_VARS;
    };

	template <typename C> 
	class ComponentRegistry COMPONENT_REGISTRY_INHERITANCE {
	COMPONENT_REGISTRY_CTOR_ACCESS:
		ComponentRegistry();
	private:
		COMPONENT_REGISTRY_VIRTUAL_IMPL(override);

		static std::unordered_map<Handle, C> s_Components;
		friend class ComponentRegistry<void>;
		friend class ECS;
		friend class ECSStatTracker;
	};

	template <>
	class ComponentRegistry<void> {
	private:
		/**
		 * @brief When a component is added we must also add a deallocator for it. 
		 * @tparam C Component Type 
		 * @param handle Entity handle
		 */
		template <typename C>
		static void OnComponentAdded(Handle handle);
		/**
		 * @brief When an entity is destroyed we must deallocate its components
		 * @param handle Entity handle. 
		 */
		static void OnEntityDestroyed(Handle handle);

		static size_t s_GlobalRegistryCount; 
		static size_t s_GlobalComponentCount;
		static std::unordered_map<Handle, std::bitset<ECS_MAX_COMPONENT_TYPES>> s_ComponentBitsets;
		static std::unordered_map<Handle, COMPONENT_REMOVER_CLASS> s_ComponentDeallocators;
	private:
		template <typename C> friend class ComponentRegistry;
		friend class ECS;
		friend class ECSStatTracker;
	};

	class Vector {
    public:
        /**
         * @brief Given a function that takes a type of 'In' and returns a type,
         *        construct a new vector of the returned type. 
         * 
         * @tparam In vector element type.       (auto deduced).
         * @tparam Fn @code Out(*)(In) @endcode  (auto deduced).
         */
        template <typename In, typename Fn> requires (CFnParamIsOfType<0, Fn, In>)
        static auto Transform(const std::vector<In>& in, Fn&& fn) -> std::vector<decltype(fn(in[0]))> {
            std::vector<decltype(fn(in[0]))> out;
            out.reserve(in.size());
            for (size_t i = 0; i < in.size(); ++i) {
                out[i] = fn(in[i]);
            }
            return out;
        }
       
        /**
         * @brief Given a type that has a constructor that takes a type of 'In',
         *        construct a new vector of the given type. 
         * 
         * @tparam Out out vector element type.  (provided by you).
         * @tparam In vector element type.       (auto deduced).
         */
        template <typename Out, typename In> requires (std::is_constructible_v<Out, In>)
        static auto Transform(const std::vector<In>& in) -> std::vector<Out> {
            std::vector<Out> out;
            out.reserve(in.size());
            for (size_t i = 0; i < in.size(); ++i) {
                out[i] = Out(in[i]);
            }
            return out;
        }
        
        /**
         * @brief Given a function that takes a type of 'In' and returns a type,
         *        construct a new vector of the returned type. Elements that do not satisify the,
         *        predicate are not added to the new vector. 
         * 
         * @tparam In vector element type.                         (auto deduced).
         * @tparam Fn @code Out(*)(In) @endcode                    (auto deduced).
         * @tparam Predicate @code bool(*)(const In& in) @endcode  (auto deduced).
         */
        template <typename In, typename Fn, typename Predicate> requires (CFnParamIsOfType<0, Fn, In>)
        static auto TransformIf(const std::vector<In>& in, Fn&& fn, Predicate&& pred) -> std::vector<decltype(fn(in[0]))> {
            std::vector<decltype(fn(in[0]))> out;
            for (const In& var : in) {
                if (pred(var)) {
                    out.push_back(fn(var));
                }                
            }
            return out;
        }
       
        /**
         * @brief Given a type that has a constructor that takes a type of 'In',
         *        construct a new vector of the given type.  Elements that do not satisify the,
         *        predicate are not added to the new vector.
         *  
         * @tparam Out out vector element type.                    (provided by you).
         * @tparam In vector element type.                         (auto deduced).
         * @tparam Predicate @code bool(*)(const In& in) @endcode  (auto deduced).
         */
        template <typename Out, typename In, typename Predicate> requires (std::is_constructible_v<Out, In>)
        static auto TransformIf(const std::vector<In>& in, Predicate&& pred) -> std::vector<Out> {
            std::vector<Out> out;
            for (const In& var : in) {
                if (pred(var)) {
                    out.push_back(Out(var));
                }
            }
            return out;
        }

    };

	template <typename T>
    class View {
    public:
        View(const View<T>& view) : m_View(view.m_View) {}
        View(const std::vector<T>& vector) : m_View(vector) {}
        View(const std::span<T>& span) : m_View(span.begin(), span.end()) {}

        std::size_t Size() const {
            return m_View.size();
        }
        
        bool Empty() const {
            return m_View.empty();
        }

        View<T> Copy() {
            return View<T>(*this);
        }

        std::span<T> Range(std::size_t begin, std::size_t end) {
            return std::span<T>(m_View.begin() + begin, m_View.begin() + end);
        }

        template <typename Predicate = bool(*)(const T&)>
        View<T>& Filter(Predicate&& pred) {
            m_View = Vector::TransformIf<T>(m_View, pred);
            return *this;
        }

        template <typename Predicate = bool(*)(const T&, const T&)> 
        View<T>& Sort(Predicate&& pred = [](const T& lhs, const T& rhs) -> bool { return lhs < rhs; }) {
            std::sort(m_View.begin(), m_View.end(), std::forward<Predicate>(pred));
            return *this;
        }

        template <template <typename> typename Comparator = std::less> 
        View<T>& Sort() {
            std::sort(m_View.begin(), m_View.end(), Comparator<T>());
            return *this;
        }
        
        explicit operator bool() {
            return !Empty();
        }
        
    public:
        auto begin() { return m_View.begin(); }
        auto begin() const { return m_View.begin(); }
        auto end() { return m_View.end(); }
        auto end() const { return m_View.end(); }
    public:
        T& operator[](std::size_t index) { return m_View[index]; }
        const T& operator[](std::size_t index) const { return m_View[index]; }
    private:
        std::vector<T> m_View;
    };
}

// Types
namespace Abyss { 

    template <typename IDType> requires (std::is_arithmetic_v<IDType>)
    class TypeCounter {
    private:
        template <typename T>
        static IDType Gen() noexcept {
            static IDType id = Start++;
            return id;
        }
        static IDType Start;
        template <typename T> friend class Type;
    };

    template <typename IDType> requires (std::is_arithmetic_v<IDType>)
    IDType TypeCounter<IDType>::Start = 0;

    template <typename T>
    class Type {
    public:
        static constexpr inline auto Name() noexcept -> std::string_view { 
            #if defined(__clang__)
                constexpr auto prefix = std::string_view{"[T = "};
                constexpr auto suffix = "]";
                constexpr auto function = std::string_view{__PRETTY_FUNCTION__};
            #elif defined(__GNUC__)
                constexpr auto prefix = std::string_view{"with T = "};
                constexpr auto suffix = "; ";
                constexpr auto function = std::string_view{__PRETTY_FUNCTION__};
            #elif defined(_MSC_VER)
                constexpr auto prefix = std::string_view{"get_type_name<"};
                constexpr auto suffix = ">(void)";
                constexpr auto function = std::string_view{__FUNCSIG__};
            #else
                # error Unsupported compiler
            #endif

            const auto start = function.find(prefix) + prefix.size();
            const auto end = function.find(suffix);
            const auto size = end - start;

            return function.substr(start, size);
        }

        static size_t Hash() noexcept { return typeid(T).hash_code(); }
        static auto ID() noexcept { return TypeCounter<uint32_t>::Gen<T>(); }
        template <typename Other>
        static bool Before() noexcept { return typeid(T).before(typeid(Other)); }

        constexpr static bool DefaultConstructible() noexcept { return std::is_default_constructible_v<T>; }
        constexpr static bool CopyConstructible() noexcept { return std::is_copy_constructible_v<T>; }
        constexpr static bool MoveConstructible() noexcept { return std::is_move_constructible_v<T>; }
        constexpr static bool CopyAssignable() noexcept{ return std::is_copy_assignable_v<T>; }
        constexpr static bool MoveAssignable() noexcept { return std::is_move_assignable_v<T>; }
        constexpr static bool Destructible() noexcept { return std::is_destructible_v<T>; }
        constexpr static bool TriviallyDefaultConstructible() noexcept { return std::is_trivially_default_constructible_v<T>; }
        constexpr static bool TriviallyCopyConstructible() noexcept { return std::is_trivially_copy_constructible_v<T>; }
        constexpr static bool TriviallyMoveConstructible() noexcept { return std::is_trivially_move_constructible_v<T>; }
        constexpr static bool TriviallyCopyAssignable() noexcept { return std::is_trivially_copy_assignable_v<T>; }
        constexpr static bool TriviallyMoveAssignable() noexcept { return std::is_trivially_move_assignable_v<T>; }
        constexpr static bool TriviallyDestructible() noexcept { return std::is_trivially_destructible_v<T>; }
        constexpr static bool IsArithmetic() noexcept { return std::is_arithmetic_v<T>; }
        constexpr static bool IsIntegral() noexcept { return std::is_integral_v<T>; }
        constexpr static bool IsFloatingPoint() noexcept { return std::is_floating_point_v<T>; }
        constexpr static bool IsPointer() noexcept { return std::is_pointer_v<T>; }
        constexpr static bool IsEnum() noexcept { return std::is_enum_v<T>; }
        constexpr static bool IsClass() noexcept { return std::is_class_v<T>; }
        constexpr static bool IsUnion() noexcept { return std::is_union_v<T>; }
        constexpr static bool IsArray() noexcept { return std::is_array_v<T>; }
        constexpr static bool IsReference() noexcept { return std::is_reference_v<T>; }
        constexpr static bool IsVoid() noexcept { return std::is_void_v<T>; }
        template <typename Other>
        constexpr static bool IsBaseOf() noexcept { return std::is_base_of_v<T, Other>; }

        constexpr static auto Max() noexcept requires (std::is_arithmetic_v<T>) {
            return std::numeric_limits<T>::max();
        }
        constexpr static auto Min() noexcept requires (std::is_arithmetic_v<T>) {
            return std::numeric_limits<T>::min();
        }
        constexpr static auto Lowest() noexcept requires (std::is_arithmetic_v<T>) {
            return std::numeric_limits<T>::lowest();
        }
        constexpr static auto Epsilon() noexcept requires (std::is_arithmetic_v<T>) {
            return std::numeric_limits<T>::epsilon();
        }
        constexpr static auto Infinity() noexcept requires (std::is_arithmetic_v<T>) {
            return std::numeric_limits<T>::infinity();
        }
        constexpr static auto Quiet_NaN() noexcept requires (std::is_arithmetic_v<T>) {
            return std::numeric_limits<T>::quiet_NaN();
        }
		constexpr static auto Quiet_2NaN() noexcept requires (std::is_arithmetic_v<T>) {
			return std::numeric_limits<T>::quiet_2NaN();
		}

        constexpr static auto Signaling_NaN() noexcept requires (std::is_arithmetic_v<T>) {
            return std::numeric_limits<T>::signaling_NaN();
        }
        constexpr static auto Denorm_Min() noexcept requires (std::is_arithmetic_v<T>) {
            return std::numeric_limits<T>::denorm_min();
        }
        constexpr static auto RoundError() noexcept requires (std::is_arithmetic_v<T>) {
            return std::numeric_limits<T>::round_error();   
        }

        static constexpr auto Null() requires (std::is_pointer_v<T>) {
            return nullptr;
        }
        static constexpr auto Null() requires (std::is_floating_point_v<T>) {
            return Type<T>::Quiet_2NaN();
        }
        static constexpr auto Null() requires (!std::is_floating_point_v<T> && std::is_arithmetic_v<T> && std::is_signed_v<T>) {
            return Type<T>::Min();
        }
        static constexpr auto Null() requires (!std::is_floating_point_v<T> && std::is_arithmetic_v<T> && std::is_unsigned_v<T>)  {
            return Type<T>::Max();
        }
        static constexpr auto Null() requires (CIsString<T>) {
            return "";
        }
        static constexpr auto Null() requires (!std::is_pointer_v<T> && !std::is_arithmetic_v<T> && !CIsString<T> && std::is_default_constructible_v<T>) {
            return T{};
        }

        template <typename Other>
        bool operator==(Type<Other> type) noexcept {
            return typeid(T) == typeid(Other);
        } 
        template <typename Other>
        bool operator!=(Type<Other> type) noexcept {
            return typeid(T) != typeid(Other);
        } 
        template <typename Other>
        bool operator>(Type<Other> type) noexcept {
            return !this->Before<Other>();
        }
        template <typename Other>
        bool operator<(Type<Other> type) noexcept {
            return this->Before<Other>();
        }
    private:
        static auto ECSID() noexcept { return TypeCounter<uint64_t>::Gen<T>(); }
        friend class ECS;
        template <typename C> friend class ComponentRegistry;
    };
    
    template <typename T> requires (std::is_arithmetic_v<T>)
    class Range {
    public:
        Range(const Range<T>& other) : 
            Min(other.Min), 
            Max(other.Max) 
        {

        }
        template <typename U> requires (!std::is_same_v<U, T>)
        Range(const Range<U>& other) : 
            Min(static_cast<T>(other.Min)),
            Max(static_cast<T>(other.Max))
        {

        }

        Range(T min = Type<T>::Min(), T max = Type<T>::Max()) : 
            Min(min), 
            Max(max)
        {

        }

        bool Contains(T value) const {
            return Max >= value && Min <= value;
        }

        bool Intersects(const Range<T>& other) const {
            return !(Max < other.Min || Min > other.Max);
        }
        
        T Random() const {
            std::random_device rd;  
            std::mt19937 gen(rd()); 
            if constexpr (std::is_integral_v<T>) {
                std::uniform_int_distribution<T> dis(Min, Max);
                return dis(gen);
            } else {
                std::uniform_real_distribution<T> dis(Min, Max);
                return dis(gen);
            }
        }

        T Clamp(T value) const {
            return std::clamp(value, Min, Max);
        }

        T Length() const {
            return Max - Min;
        }

        T MidPoint() const {
            return (Min + Max) / 2;
        }

    public:
        bool operator==(const Range<T>& other) const {
            return Min == other.Min && Max == other.Max;
        }

        // Inequality operator
        bool operator!=(const Range<T>& other) const {
            return !(*this == other);
        }
    public:
        T Min;
        T Max;
    };
   
}

// std::ostream overloads
namespace Abyss {
    template <typename T>
    static std::ostream& operator<<(std::ostream& os, Type<T> type) {
        os << "Type : {" << "\n";
        os << "\t" << "Name : " << type.Name() << "\n";
        os << "\t" << "ID   : " << type.ID()   << "\n";
        os << "\t" << "Hash : " << type.Hash() << "\n";
        os << "\t" << "Bit  : " << type.Bit()  << "\n";
        return os << "}";
    }
}

// Component Remover
namespace Abyss {
    void COMPONENT_REMOVER_CLASS::Push(COMPONENT_REMOVER_PUSH_ARGS) {
	#ifdef ECS_USE_FN_COMP_DESTRUCTION
		m_CleanupFns.push(fn);
	#else
		m_Registries.push_back(std::move(registry));
			
	#endif
	}

	void COMPONENT_REMOVER_CLASS::Flush(Handle handle) {
	#ifdef ECS_USE_FN_COMP_DESTRUCTION
		while (!m_CleanupFns.empty()) {
			m_CleanupFns.top()();
			m_CleanupFns.pop();
		}
	#else
		for (auto& registry : m_Registries) {
			ECS_LOG_DEBUG("Destroying component from ", registry->Name(), " owned by entity : ", handle);
			registry->Destroy(handle);
		}
	#endif
	}
	
}

// Component Registry
namespace Abyss {

    template <typename C>
    std::unordered_map<Handle, C> ComponentRegistry<C>::s_Components = {};

    size_t ComponentRegistry<void>::s_GlobalRegistryCount = 0;
    size_t ComponentRegistry<void>::s_GlobalComponentCount = 0;
    std::unordered_map<Handle, COMPONENT_REMOVER_CLASS> ComponentRegistry<void>::s_ComponentDeallocators = {};
	std::unordered_map<Handle, std::bitset<ECS_MAX_COMPONENT_TYPES>> ComponentRegistry<void>::s_ComponentBitsets = {};

	template <typename C>
	ComponentRegistry<C>::ComponentRegistry() {
		++ComponentRegistry<void>::s_GlobalRegistryCount;
	}




#ifndef ECS_USE_FN_COMP_DESTRUCTION
	template <typename C>
	void ComponentRegistry<C>::Destroy(Handle handle) {
		s_Components.erase(handle);
	}

	template <typename C>
	std::string_view  ComponentRegistry<C>::Name() {
 		return Type<ComponentRegistry<C>>::Name();
	}
#endif
	
    template <typename C>
    void ComponentRegistry<void>::OnComponentAdded(Handle handle) {
	#ifdef ECS_USE_FN_COMP_DESTRUCTION
        s_ComponentDeallocators[handle].Push([&](){
			ECS_LOG_DEBUG("Deallocating component of type '", Type<C>::Name(), '\'');
			ComponentRegistry<C>::s_Components.erase(handle);
			--s_GlobalComponentCount;
		});
	#else
		s_ComponentDeallocators[handle].Push(std::make_unique<ComponentRegistry<C>>());
	#endif
		s_ComponentBitsets[handle][Type<C>::ECSID()] = true;

		++s_GlobalComponentCount;
		ECS_LOG_DEBUG("Component of type '", Type<C>::Name(), "' added to entity : ", handle);
    }

	void ComponentRegistry<void>::OnEntityDestroyed(Handle handle) {
		s_ComponentDeallocators[handle].Flush(handle);
		s_ComponentDeallocators.erase(handle);		
	}


}

// ECS
namespace Abyss {
    std::vector<Handle> ECS::s_Handles = {};

	Entity ECS::Create() {
		static Handle handle = 0;
		Entity entity(handle);
		s_Handles.push_back(handle);
		ECS_LOG_DEBUG("New Entity Created : ", handle);
		++handle;
		return entity;
	}
	
	bool ECS::Destroy(Entity entity) {
		auto it = std::find(s_Handles.begin(), s_Handles.end(), entity.m_Handle);
		if (it != s_Handles.end()) {
			ComponentRegistry<void>::OnEntityDestroyed(entity.m_Handle);
			s_Handles.erase(it);
			return true;
		}
		ECS_LOG_ERROR("Entity does not exist : ", entity.m_Handle);
		return false;
	}

    void ECS::Clear() {
		auto handles = s_Handles;
		for (const auto& handle : handles) {
			ComponentRegistry<void>::OnEntityDestroyed(handle);
		}
		s_Handles.clear();
		ECS_LOG_DEBUG("All Entites cleared");
	}

	size_t ECS::Size() {
		return s_Handles.size();
	}
	
	void ECS::SetLogger(std::unique_ptr<ILogger> logger) {
		ILogger::SetLogger(std::move(logger));
	} 
	
	Abyss::View<Entity> ECS::View(size_t begin, size_t end) {
		if (begin > end || end >= s_Handles.size()) {
			throw std::out_of_range("Indices are out of bounds.");
		}

		auto entities = Vector::Transform<Entity>(s_Handles);
		return std::vector<Entity>(entities.begin() + begin, entities.begin() + end + 1);
	}


	Abyss::View<Entity> ECS::View() {
		return Abyss::View<Entity>(Vector::Transform<Entity>(s_Handles));
	}

	template <typename C>
	Abyss::View<Entity> ECS::View() {
		return Vector::TransformIf<Entity>(s_Handles, ECS::HasComponent<C>);
	}
	
	template <typename ...Cs>  requires (CArgCountGreater<1, Cs...>) 
	Abyss::View<Entity> ECS::View(){
		return Vector::TransformIf<Entity>(s_Handles, ECS::HasComponents<Cs...>);
	}

	template <typename ...Cs> requires (CArgCountGreater<0, Cs...>)
	Abyss::View<Entity> ECS::Group() {
		return Vector::TransformIf<Entity>(s_Handles, 
			[&](const Handle& handle) {
            	return (ECS::HasComponent<Cs>(Entity(handle)) || ...);
        	});
	}

	template <typename Fn> requires (CFnParamIsOfType<0, Fn, Entity>)
	void ECS::Iterate(Fn&& fn) {
		for (auto entity : ECS()) {
			fn(entity);
		}
	}
	
	std::unordered_map<ECS::EStatistics, ECS::SStat> ECS::Statistics() {
		return std::unordered_map<ECS::EStatistics, ECS::SStat> {
			{EStatistics::ENTITIES,        { "Entities", s_Handles.size() }},
			{EStatistics::REGISTRIES,      { "Registries", ComponentRegistry<void>::s_GlobalRegistryCount }},
			{EStatistics::COMPONENTS,      { "Components", ComponentRegistry<void>::s_GlobalComponentCount }},
		}; 
	}

	ECS::iterator ECS::begin() noexcept {
		return iterator(s_Handles.begin());
	}
	
	ECS::iterator ECS::end() noexcept {
		return iterator(s_Handles.end());
	}

	ECS::const_iterator ECS::begin() const noexcept {
		return const_iterator(s_Handles.cbegin());
	}

	ECS::const_iterator ECS::end() const noexcept {
		return const_iterator(s_Handles.cend());
	}	
	
	template <typename C> 
	void ECS::AddComponent(Entity entity, const C& component) requires (std::is_copy_constructible_v<C>) {
		static ComponentRegistry<C> registry;
		ECS_ASSERT(!ComponentExists<C>(entity), "Component already exists!");
		registry.s_Components[entity.m_Handle] = component;
		ComponentRegistry<void>::OnComponentAdded<C>(entity.m_Handle);
	}

	template <typename C> 
	void ECS::AddComponent(Entity entity) requires (std::is_default_constructible_v<C>) {
		AddComponent<C>(entity, C());
	}

	template <typename C, typename ...Args>
	void ECS::AddComponent(Entity entity, Args... args) requires (std::is_constructible_v<C, Args...>) {
		AddComponent<C>(entity, C(args...));
	}

	template <typename C>
	void ECS::RemoveComponent(Entity entity) {
		auto& components = ComponentRegistry<C>::s_Components;
		ECS_ASSERT(ComponentExists<C>(entity), "Component does not exist!");
		components.erase(entity.m_Handle);
		ECS_LOG_DEBUG("Component of type '", Type<C>::Name(), "' removed from entity :", entity.m_Handle);
	}

	template <typename C>
	C& ECS::GetComponent(Entity entity) {
		ECS_ASSERT(ECS::ComponentExists<C>(entity), "Component does not exist");
		return ComponentRegistry<C>::s_Components[entity.m_Handle];
	}

	template<typename ...Cs>
	auto ECS::GetComponents(Entity entity) -> std::tuple<Cs&...> {
		return std::tie(ECS::GetComponent<Cs>(entity)...);
	}


	template <typename C>
	bool ECS::HasComponent(Entity entity) {
		return ComponentRegistry<void>::s_ComponentBitsets[entity.m_Handle][Type<C>::ECSID()];
	}

	template <typename ...Cs>
	bool ECS::HasComponents(Entity entity) {
		return (true && ... && ECS::HasComponent<Cs>(entity));
	}

	template <typename C>
	bool ECS::ComponentExists(Entity entity) {
		return (ComponentRegistry<C>::s_Components.find(entity.m_Handle) != ComponentRegistry<C>::s_Components.end());
	}

	size_t ECS::Components(Entity entity) {
		return ComponentRegistry<void>::s_ComponentBitsets[entity.m_Handle].count();
	}
}

// Entity
namespace Abyss {
	Entity::Entity() :
		m_Handle(Type<Abyss::Handle>::Null())
	{

	}

	Entity::Entity(const Entity& other) :
		m_Handle(other.m_Handle)
	{

	}

	Entity::Entity(Abyss::Handle handle) : 
		m_Handle(handle) 
	{

	}

	template <typename C>
	void Entity::AddComponent(const C& component) requires (std::is_copy_constructible_v<C>) {
		ECS::AddComponent<C>(*this, component);
	}
	
	template <typename C> 
	void Entity::AddComponent() requires (std::is_default_constructible_v<C>) {
		ECS::AddComponent<C>(*this);
	}
	
	template <typename C, typename ...Args> 
	void Entity::AddComponent(Args... args) requires (std::is_constructible_v<C, Args...>) {
		ECS::AddComponent<C>(*this, args...);
	}	

	void Entity::Destroy() {
		ECS::Destroy(*this);
	}

	template <typename C>
	void Entity::RemoveComponent() {
		ECS::RemoveComponent<C>(*this);
	}

	template <typename C>
	const C& Entity::GetComponent() const {
		return ECS::GetComponent<C>(*this);
	}

	template <typename C>
	C& Entity::GetComponent() {
		return ECS::GetComponent<C>(*this);
	}

	template <typename ...Cs>
	auto Entity::GetComponents() -> std::tuple<Cs&...> {
		return ECS::GetComponents<Cs...>(*this);
	}

	template <typename ...Cs>
	auto Entity::GetComponents() const -> const std::tuple<Cs&...> {
		return ECS::GetComponents<Cs...>(*this);
	}


	template <typename C>
	bool Entity::HasComponent() const {
		return ECS::HasComponent<C>(*this);
	}

	template <typename ...Cs>
	bool Entity::HasComponents() const {
		return ECS::HasComponents<Cs...>(*this);
	}

	size_t Entity::Components() const {
		return ECS::Components(*this);
	}

	Entity::operator bool() const { 
		return this->m_Handle != Type<::Abyss::Handle>::Max();
	}
}

