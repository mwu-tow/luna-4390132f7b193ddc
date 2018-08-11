#define _SILENCE_CXX17_ITERATOR_BASE_CLASS_DEPRECATION_WARNING
#include "xlsx.h"

#include "Core/Common.h"
#include "Core/Error.h"

#include <arrow/builder.h>
#include <arrow/table.h>

#ifdef DISABLE_XLSX

#else // DISABLE_XLSX

#include <xlnt/xlnt.hpp>

namespace
{
    using namespace std::literals;

    struct ColumnBuilderBase
    {
        virtual void addFromCell(const xlnt::cell &field) = 0;
        virtual void addMissing() = 0;
        virtual void reserve(int64_t count) = 0;
        virtual std::shared_ptr<arrow::Array> finish() = 0;
    };

    template<arrow::Type::type type>
    struct ColumnBuilder : ColumnBuilderBase
    {
        bool nullable{};
        BuilderFor<type> builder;

        ColumnBuilder(bool nullable) : nullable(nullable) {}

        virtual void addFromCell(const xlnt::cell &field) override
        {
            if(field.has_value())
            {
                if constexpr(type == arrow::Type::STRING)
                    builder.Append(field.to_string());
                else if constexpr(type == arrow::Type::INT64)
                    builder.Append(field.value<int64_t>());
                else if constexpr(type == arrow::Type::DOUBLE)
                    builder.Append(field.value<double>());
                else
                    throw std::runtime_error("wrong type");
            }
            else
                addMissing();
        }
        virtual void addMissing() override
        {
            if(nullable)
                builder.AppendNull();
            else
                builder.Append(defaultValue<type>());
        }
        virtual void reserve(int64_t count) override
        {
            builder.Reserve(count);
        }

        virtual std::shared_ptr<arrow::Array> finish() override
        {
            return ::finish(builder);
        }
    };


//     void xlsxPrintToFile(const Matrix2d &matrix, const char *filename)
//     {
//         xlnt::workbook wb;
//         auto sheet = wb.active_sheet();
//         sheet.title("Written from Luna");
// 
//         for(auto row = 0ull; row < matrix.rowCount; row++)
//         {
//             for(auto column = 0ull; column < matrix.columnCount; column++)
//             {
//                 // translate indices to from-1-indexed xlnt types
//                 const auto xlsRow = static_cast<xlnt::row_t>(row + 1);
//                 const auto xlsColumn = static_cast<xlnt::column_t::index_t>(column + 1);
// 
//                 const auto cellContents = matrix.load(row, column);
//                 sheet.cell(xlsColumn, xlsRow).value(cellContents);
//             }
//         }
// 
//         wb.save(filename);
//     }
}

std::shared_ptr<arrow::Table> readXlsxFile(const char *filepath, HeaderPolicy header, std::vector<ColumnType> columnTypes)
{
    try
    {
        xlnt::workbook wb;
        wb.load(filepath);
        const auto sheet = wb.active_sheet();

        // We keep the object under unique_ptr, so there will be
        // no leak if exception is thrown before the end of function
        const auto rowCount = sheet.highest_row();
        const auto columnCount = sheet.highest_column().index;

        // If there is no type info for column, default to non-nullable Text (it always works)
        if(columnTypes.size() < columnCount)
        {
            const ColumnType nonNullableText{ std::make_shared<arrow::StringType>(), false };
            columnTypes.resize(columnCount, nonNullableText);
        }
        const auto names = decideColumnNames(columnCount, header, [&](int column)
        {
            return sheet.cell(column + 1, 0 + 1).to_string();
        });
        const bool useFirstRowAsHeaders = nonstd::holds_alternative<TakeFirstRowAsHeaders>(header);

        // setup column builders
        std::vector<std::unique_ptr<ColumnBuilderBase>> columnBuilders;
        for(auto columnType : columnTypes)
        {
            auto ptr = [&] () -> std::unique_ptr<ColumnBuilderBase>
            {
                switch(columnType.type->id())
                {
                case arrow::Type::INT64:
                    return std::make_unique<ColumnBuilder<arrow::Type::INT64>>(columnType.nullable);
                case arrow::Type::DOUBLE:
                    return std::make_unique<ColumnBuilder<arrow::Type::DOUBLE>>(columnType.nullable);
                case arrow::Type::STRING:
                    return std::make_unique<ColumnBuilder<arrow::Type::STRING>>(columnType.nullable);
                }

            }();
            ptr->reserve(rowCount);
            columnBuilders.push_back(std::move(ptr));
        }
        for(auto i = columnBuilders.size(); i < columnCount; i++)
            columnBuilders.push_back(std::make_unique<ColumnBuilder<arrow::Type::STRING>>(false));
    
        for(int column = 0; column < columnCount; column++)
        {
            for(int row = useFirstRowAsHeaders; row < rowCount; row++)
            {
                xlnt::cell_reference cellPos(column+1, row+1);
                if(sheet.has_cell(cellPos))
                    columnBuilders[column]->addFromCell(sheet.cell(cellPos));
                else
                    columnBuilders[column]->addMissing();
            }
        }

        std::vector<std::shared_ptr<arrow::Array>> arrays;
        for(auto &builder : columnBuilders)
            arrays.push_back(builder->finish());


        return buildTable(names, arrays, columnTypes);

    }
    catch(std::exception &e)
    {
        throw std::runtime_error("Failed to parse file `"s + filepath + "` : " + e.what());
    }
}

#endif // DISABLE_XLSX