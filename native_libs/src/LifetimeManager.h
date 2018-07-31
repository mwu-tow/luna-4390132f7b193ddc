#pragma once

// NOTE: this a third-party C++17 std::any implementation - providing nonstd::any. 
// When C++17 is available, it will default to std::any, otherwise it will provide
// its own header-only implementation with the same interface.
//
// To be removed and replaced with std::any when C++17 complaint library is available
// on Mac platforms we want to be compiled upon.
#include <nonstd/any.hpp>

#include <memory>
#include <mutex>
#include <sstream>
#include <unordered_map>
// TODO? could get rid of most headers by using pimpl

// Class is meant as a helper for managing std::shared_ptr lifetimes when they are shared
// with a foreign language through C API.
// Each time when shared_ptr is moved to foreign code it should be done through `addOwnership`
// When foreign code is done with the pointer, `releaseOwnership` should be called.
//
// Storage is thread-safe (internally synchronized with a lock).
//
// Technically there's nothing shared_ptr specific in storage (it uses type-erased any),
// if needed it can be adjusted to work with other kinds of types with similar semantics.
class LifetimeManager
{
    mutable std::mutex mx;
    std::unordered_multimap<void *, nonstd::any> storage; // address => shared_ptr<T>

    // Looks up the pointer and calls the given function with storage iterator (while having the storage lock).
    template<typename Function>
    auto access(void *ptr, Function &&f) const
    {
        std::unique_lock<std::mutex> lock{ mx };
        if(auto itr = storage.find(ptr); itr != storage.end())
        {
            return f(itr);
        }

        std::ostringstream out;
        out << "Cannot find storage for pointer " << ptr << " -- was it previously registered?";
        throw std::runtime_error(out.str());
    }

public:
    LifetimeManager();
    ~LifetimeManager();

    template<typename T>
    T *addOwnership(std::shared_ptr<T> ptr)
    {
        auto ret = ptr.get();
        std::unique_lock<std::mutex> lock{ mx };
        storage.emplace(ret, std::move(ptr));
        return ret;
    }
    void releaseOwnership(void *ptr)
    {
        access(ptr, [this] (auto itr)
        {
            // TODO should separate retrieving any from storage and deleting it
            // deleting can take time and lock is not needed then
            storage.erase(itr);
        });
    }

    // NOTE: be careful, as this does not handle shared_ptr casting (type should exactly match)
    template<typename T>
    std::shared_ptr<T> accessOwned(void *ptr) const
    {
        return access(ptr, [&] (auto itr)
        {
            return nonstd::any_cast<std::shared_ptr<T>>(itr->second);
        });
    }
    template<typename T>
    std::shared_ptr<T> accessOwned(T *ptr) const
    {
        return accessOwned<T>(static_cast<void*>(ptr));
    }

    // TODO reconsider at some stage more explicit global state
    static auto &instance()
    {
        static LifetimeManager manager;
        return manager;
    }
};
