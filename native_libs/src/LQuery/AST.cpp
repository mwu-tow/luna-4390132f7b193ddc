#include "AST.h"

#include <cassert>
#include <iostream>
#include <vector>

#include <arrow/table.h>
#include <arrow/type_traits.h>

#define RAPIDJSON_NOMEMBERITERATORCLASS 
#include <rapidjson/document.h>
#include <rapidjson/error/en.h>
#include <rapidjson/writer.h>
#include <rapidjson/stringbuffer.h>

#include "Core/ArrowUtilities.h"

#include "variant.h"

using namespace std::literals;

namespace
{
std::string toJsonString(const rapidjson::Value &v)
{
    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    v.Accept(writer);
    return buffer.GetString();
}
struct DslParser
{
    DslParser(const arrow::Table &table) : table(table)
    {
    }

    const arrow::Table &table;
    std::unordered_map<std::string, int> columnNamesUsed;
    ColumnMapping columnMapping;

    ColumnIndexInTable requiredColumnIndex(const std::string &name) const
    {
        for(int i = 0; i < table.num_columns(); i++)
            if(table.column(i)->name() == name)
                return i;

        throw std::runtime_error("cannot find column by name `" + name + "`");
    }

    auto parseOperands(const rapidjson::Value &v)
    {
        if(!v.IsArray())
            throw std::runtime_error("cannot parse operands from non-list json element: " + toJsonString(v));

        const auto arguments = v.GetArray();
        std::array<std::unique_ptr<ast::Value>, ast::MaxOperatorArity> operands;
        for(int i = 0; i < (int)arguments.Size(); i++)
            operands.at(i) = std::make_unique<ast::Value>(parseValue(arguments[i]));

        return operands;
    }
    auto parsePredicates(const rapidjson::Value &v)
    {
        if(!v.IsArray())
            throw std::runtime_error("cannot parse operands from non-list json element: " + toJsonString(v));

        const auto arguments = v.GetArray();
        std::array<std::unique_ptr<ast::Predicate>, ast::MaxOperatorArity> operands;
        for(int i = 0; i < (int)arguments.Size(); i++)
            operands.at(i) = std::make_unique<ast::Predicate>(parsePredicate(arguments[i]));

        return operands;
    }

    ast::Value parseValue(const rapidjson::Value &v)
    {
        if(v.IsObject())
        {
            const auto obj = v.GetObject();
            if(obj.HasMember("column") && obj["column"].IsString())
            {
                const auto columnName = obj["column"].GetString();
                if(auto nameItr = columnNamesUsed.find(columnName); 
                    nameItr != columnNamesUsed.end())
                {
                    return ast::ColumnReference{nameItr->second};
                }
                else
                {
                    const ColumnIndexInTable columnIndex = requiredColumnIndex(columnName);
                    const ColumnReferenceId referenceIndex = (int)columnNamesUsed.size();
                    columnNamesUsed[columnName] = referenceIndex;
                    columnMapping[referenceIndex] = columnIndex;
                    return ast::ColumnReference{referenceIndex};
                }
            }
            else if(obj.HasMember("operation") && obj["operation"].IsString() 
                && obj.HasMember("arguments") && obj["arguments"].IsArray())
            {
                const auto operationName = obj["operation"].GetString();
                const auto operationCode = ast::valueOperatorFromName(operationName);
                auto operands = parseOperands(obj["arguments"]);
                return ast::ValueOperation{operationCode, std::move(operands)};
            }
        }
        else if(v.IsFloat())
        {
            return ast::Literal<double>{v.GetFloat()};
        }
        else if(v.IsInt64())
        {
            return ast::Literal<int64_t>{v.GetInt64()};
        }
        else if(v.IsString())
        {
            return ast::Literal<std::string>{v.GetString()};
        }
        throw std::runtime_error("Failed to parse LQuery value from: " + toJsonString(v));
    }


    ast::Predicate parsePredicate(const rapidjson::Value &v)
    {
        using namespace ast;
        using namespace rapidjson;

        if(!v.IsObject())
            throw std::runtime_error("Cannot parse predicate: must be a JSON object");

        if(const auto pred = v.FindMember("predicate"); 
            pred != v.MemberEnd() && pred->value.IsString())
        {
            if(const auto args = v.FindMember("arguments"); 
                args != v.MemberEnd() && args->value.IsArray())
            {
                const auto predFromValueOperator = predicateOperatorFromName(pred->value.GetString());
                auto operands = parseOperands(args->value);
                return PredicateFromValueOperation{predFromValueOperator, std::move(operands)};
            }
        }
        else if(const auto pred = v.FindMember("boolean"); 
            pred != v.MemberEnd() && pred->value.IsString())
        {
            if(const auto args = v.FindMember("arguments"); 
                args != v.MemberEnd() && args->value.IsArray())
            {
                const auto predFromValueOperator = predicateBooleanOperatorFromName(pred->value.GetString());
                auto operands = parsePredicates(args->value);
                return PredicateOperation{predFromValueOperator, std::move(operands)};
            }
        }

        throw std::runtime_error("Failed to parse LQuery predicate from: " + toJsonString(v));
    }

    ast::Predicate parsePredicate(const char *dslInJsonText)
    {
        using namespace ast;
        using namespace rapidjson;

        Document doc{};
        doc.Parse(dslInJsonText);

        if(doc.HasParseError())
            throw std::runtime_error("Failed to parse JSON: "s + GetParseError_En(doc.GetParseError()));

        return parsePredicate(doc);
    }
};

}

namespace ast
{

    ast::ValueOperator valueOperatorFromName(const std::string &name)
    {
        static const std::unordered_map<std::string, ValueOperator> map
        {
            {"plus"  , ValueOperator::Plus  },
            {"minus" , ValueOperator::Minus },
            {"times" , ValueOperator::Times },
            {"divide", ValueOperator::Divide},
            {"negate", ValueOperator::Negate},
        };

        if(auto itr = map.find(name); itr != map.end())
            return itr->second;

        throw std::runtime_error("unknown value operation: `" + name + "`");
    }

    ast::PredicateOperator predicateBooleanOperatorFromName(const std::string &name)
    {
        static const std::unordered_map<std::string, PredicateOperator> map
        {
            {"or"  , PredicateOperator::Or  },
            {"and" , PredicateOperator::And },
            {"not" , PredicateOperator::Not },
        };

        if(auto itr = map.find(name); itr != map.end())
            return itr->second;

        throw std::runtime_error("unknown value operation: `" + name + "`");
    }

    ast::PredicateFromValueOperator predicateOperatorFromName(const std::string &name)
    {
        static const std::unordered_map<std::string, PredicateFromValueOperator> map
        {
            {"gt" , PredicateFromValueOperator::Greater  },
            {"lt" , PredicateFromValueOperator::Lesser },
            {"eq" , PredicateFromValueOperator::Equal },
            {"startsWith" , PredicateFromValueOperator::StartsWith },
            {"matches" , PredicateFromValueOperator::Matches },
        };

        if(auto itr = map.find(name); itr != map.end())
            return itr->second;

        throw std::runtime_error("unknown predicate operation: `" + name + "`");
    }

    std::pair<ColumnMapping, Predicate> parsePredicate(const arrow::Table &table, const char *lqueryJsonText)
    {
        DslParser parser{table};
        auto pred = parser.parsePredicate(lqueryJsonText);
        return std::make_pair(parser.columnMapping, std::move(pred));
    }
}
