// Fabian Dehnbostel 260AIM015
// https://github.com/RHdehn/calc-lab-home
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used: J. Wolf: C von A bis Z, Rheinwerk Openbook,link:https://openbook.rheinwerk-verlag.de/c_von_a_bis_z/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>



#define MAX_PARSER_DEPTH 1000

#define INITIAL_BUFFER_SIZE 4096




// Integer and float values stay separate so the required arithmetic type is preserved.
typedef struct {

    int is_float;
    long long integer_value;
    double float_value;
} Value;

// Absolute indexes make every error position count from the start of the whole file.


typedef struct {
    const char *input;
    size_t position;
    size_t line_end;
    size_t error_position;
    int has_error;
} Parser;


static  Value make_integer_value(long long integer_value)
{
    Value result = {0, integer_value, (double)integer_value};

    return result;

}

static Value make_float_value(double float_value) {
    Value result = {1, 0, float_value};
    return result;


}

// A mixed calculation converts the integer value to double.

static double value_as_double(Value value) {
    if (value.is_float) {
        return value.float_value;
    }

    return (double)value.integer_value;
}

// The position is stored once so only the first error is reported.
static void set_error(Parser *parser, size_t error_position){

    if(!parser->has_error){
        parser->has_error = 1;
        parser->error_position = error_position;

    }

}

// Newlines are not skipped because missing operands are reported at the line end.

static void skip_whitespace(Parser *parser){
    while(parser->position < parser->line_end &&
           (parser->input[parser->position] == ' ' ||
            parser->input[parser->position] == '\t' ||
            parser->input[parser->position] == '\r')) {
        parser->position++;
    }


}

// The parser functions follow the grammar levels so operator precedence is handled correctly.
static Value parse_expression(Parser *parser, int depth);

// Separate operator and operand positions report each error at the required place.
static Value apply_operator(Parser *parser, char operator_character,
                            Value left_value, Value right_value,
                            size_t operator_position,
                            size_t right_operand_position) {
    double left_number;
    double right_number;
    double result;

    if(parser->has_error){
        return make_integer_value(0);
    }

    // Division by zero is reported at the start of the right operand.
    if (operator_character == '/' &&
        ((right_value.is_float && right_value.float_value == 0.0) ||
         (!right_value.is_float && right_value.integer_value == 0))) {
        set_error(parser, right_operand_position);
        return make_integer_value(0);
    }

    if (operator_character == '+' || operator_character == '-' ||
        operator_character == '*' || operator_character == '/') {

        // Two integers use integer arithmetic; every mixed operation uses double arithmetic.
        if (!left_value.is_float && !right_value.is_float){
            if (operator_character == '+'){

                return make_integer_value(left_value.integer_value + right_value.integer_value);
            }

            if (operator_character == '-') {
                return make_integer_value(left_value.integer_value - right_value.integer_value);
            }
            if (operator_character == '*') {
                return make_integer_value(left_value.integer_value * right_value.integer_value);
            }
            return make_integer_value(left_value.integer_value / right_value.integer_value);
        }

        left_number= value_as_double(left_value);
        right_number =value_as_double(right_value);

        if(operator_character == '+') {
            result = left_number + right_number;
        } else if (operator_character == '-'){
            result = left_number - right_number;
        } else if (operator_character == '*') {
            result = left_number * right_number;

        } else {
            result = left_number / right_number;
        }

        // A non-finite float result is reported at the operator position.
        if(!isfinite(result))  {
            set_error(parser, operator_position);
            return make_integer_value(0);

        }

        return make_float_value(result);
    }

    // P is an internal marker because the input operator ** contains two characters.
    if (operator_character == 'P') {
        if (!left_value.is_float && !right_value.is_float) {
            long long base = left_value.integer_value;
            long long exponent = right_value.integer_value;
            long long integer_result = 1;
            // A negative integer exponent is reported at the start of the exponent.
            if (exponent <0){

                set_error(parser, right_operand_position);
                return make_integer_value(0);
            }

            // Halving the exponent reduces the number of required multiplications.
            while (exponent > 0) {
                if (exponent % 2 == 1) {
                    integer_result = integer_result * base;
                }
                exponent = exponent / 2;
                if (exponent > 0) {
                    base = base * base;
                }
            }
            return make_integer_value(integer_result);
        }

        left_number = value_as_double(left_value);
        right_number = value_as_double(right_value);
        result = pow(left_number, right_number);

        // A non-finite power result is reported at the operator position.
        if (!isfinite(result)) {
            set_error(parser, operator_position);
            return make_integer_value(0);
        }
        return make_float_value(result);
    }



    set_error(parser, operator_position);

    return make_integer_value(0);
}



// This level reads numbers and parentheses the basic parts of the grammer.
static Value parse_primary(Parser *parser, int depth) {
    size_t number_start;
    size_t number_end;
    size_t digit_position;
    int contains_decimal_point = 0;
    char *conversion_end;
    Value result;


    skip_whitespace(parser);
    if (parser->has_error) {
        return make_integer_value(0);
    }



    // line_end + 1 is the newline position or one place after a file withouta final newline.
    if (parser->position >= parser->line_end) {
        set_error(parser, parser->line_end + 1);
        return make_integer_value(0);


    }


    if (parser->input[parser->position] == '(') {
        if (depth >= MAX_PARSER_DEPTH) {
            set_error(parser, parser->position + 1);
            return make_integer_value(0);
        }


        parser->position++;

        result = parse_expression(parser, depth + 1);
        skip_whitespace(parser);

        if (!parser->has_error) {
            if (parser->position >= parser->line_end) {
                set_error(parser, parser->line_end + 1);
            } else if (parser->input[parser->position] != ')') {
                set_error(parser, parser->position + 1);
            } else {
                parser->position++;
            }
        }
        return result;
    }

    // No unary operators exist every number must start with a digit.
    if (!isdigit((unsigned char)parser->input[parser->position])) {
        set_error(parser, parser->position + 1);
        return make_integer_value(0);
    }

    number_start = parser->position ;

    while (parser->position < parser->line_end &&
           isdigit((unsigned char)parser->input[parser->position])) {
        parser->position++;
    }

    if (parser->position < parser->line_end && parser->input[parser->position] == '.') {
        contains_decimal_point = 1;
        parser->position++;

        // A digit must follow the decimal point because the required float format needs it.
        if (parser->position >= parser->line_end ||
            !isdigit((unsigned char)parser->input[parser->position])) {
            set_error(parser, parser->position);
            return make_integer_value(0) ;
        }



        while(parser->position < parser->line_end &&
               isdigit((unsigned char)parser->input[parser->position])){
            parser->position++;
        }

    }


    number_end = parser->position;
    errno = 0;

    if (contains_decimal_point) {
        // strtod is used only after the parse had checked the required float format.
        double float_number = strtod(parser->input + number_start, &conversion_end);
        if(errno == ERANGE||
            (size_t)(conversion_end - parser->input) != number_end ||
            !isfinite(float_number)) {
            set_error(parser, number_start + 1);
            return make_integer_value(0);
        }

        return make_float_value(float_number);
    }


    {
        long long integer_number = 0;
        // Eachw digit is added from left to right to build the long long value.
        for (digit_position = number_start; digit_position < number_end; digit_position++) {
            integer_number = integer_number * 10 +
                             (long long)(parser->input[digit_position] - '0');
        }
        return make_integer_value(integer_number);
    }

}

// The right side calls parse_power again, so ** is right-associative.
static Value parse_power(Parser *parser, int depth){
    Value left_value = parse_primary(parser, depth);
    size_t operator_position;
    size_t right_operand_position;
    Value right_value;


    skip_whitespace(parser);
    if (!parser->has_error && parser->position + 1 < parser->line_end &&
        parser->input[parser->position] == '*' &&
        parser->input[parser->position + 1] == '*') {
        operator_position = parser->position + 1;
        parser->position = parser->position + 2;
        skip_whitespace(parser);
        right_operand_position = parser->position + 1;


        // The complete right power is read before the current power is calculated.
        right_value = parse_power(parser, depth);
        return apply_operator(parser, 'P', left_value, right_value,
                              operator_position, right_operand_position);
    }
    return left_value;
}



// parse_power is called first, so ** has higher precedence than * and /.
static Value parse_term(Parser *parser, int depth){
    Value result = parse_power(parser, depth);


    // The loop reads * and / from left to right, so both operators are left-associative.
    while (!parser->has_error){
        char operator_character;
        size_t operator_position;
        size_t right_operand_position;
        Value right_value;


        skip_whitespace(parser);
        if (parser->position >= parser->line_end) {
            break;}

        if (parser->input[parser->position] != '*' &&
            parser->input[parser->position] != '/') {
            break;
        }

        if (parser->input[parser->position] == '*' &&
            parser->position + 1 < parser->line_end &&
            parser->input[parser->position + 1] == '*') {
            break;
        }

        operator_position = parser->position + 1;
        operator_character = parser->input[parser->position];
        parser->position++;

        skip_whitespace(parser);
        right_operand_position = parser->position + 1;
        right_value = parse_power(parser, depth);
        result = apply_operator(parser, operator_character, result, right_value,
                                operator_position, right_operand_position);
    }
    return result;
}

// parse_term is called first, so * and / have higher precedence than + and -
static Value parse_expression(Parser *parser, int depth) {
    Value result = parse_term(parser, depth);

    // The loop read + and - from left to right, so both operators are left-associative
    while (!parser->has_error) {
        char operator_character;
        size_t operator_position;
        size_t right_operand_position;
        Value right_value;

        skip_whitespace(parser);
        if (parser->position >= parser->line_end) {
            break;
        }
        if (parser->input[parser->position] != '+' &&
            parser->input[parser->position] != '-') {
            break;
        }

        operator_position = parser->position + 1;
        operator_character = parser->input[parser->position];
        parser->position++;
        skip_whitespace(parser);
        right_operand_position = parser->position + 1;
        right_value = parse_term(parser, depth);
        result = apply_operator(parser, operator_character, result, right_value,
                                operator_position, right_operand_position);
    }
    return result;
}



static char *read_entire_file(const char *file_path, size_t *file_length){
    FILE *input_file;
    char *file_content;
    size_t capacity = INITIAL_BUFFER_SIZE;
    size_t bytes_read;

    // Binary mode keeps file positions consistent because every byte is counted.
    input_file = fopen(file_path, "rb");
    if (input_file == NULL) {
        return NULL;
    }



    file_content = malloc(capacity + 1);
    if (file_content == NULL) {
        fclose(input_file);
        return NULL;
    }



    *file_length = 0;
    while (1) {
        if (*file_length == capacity) {
            char *larger_content;
            capacity = capacity * 2;
            
            larger_content = realloc(file_content, capacity + 1);
            if (larger_content == NULL) {
                free(file_content);
                fclose(input_file);
                return NULL;

            }
            file_content = larger_content;
        }


        bytes_read = fread(file_content + *file_length, 1,
                           capacity - *file_length, input_file);
        *file_length = *file_length + bytes_read;

        if (bytes_read == 0) {
            if (ferror(input_file)) {
                free(file_content);
                fclose(input_file);
                return NULL;
            }
            break;
        }
    }

    file_content[*file_length] = '\0';
    fclose(input_file);
    return file_content;
}

// The first pass finds errors so no partial results are written.
static int evaluate_lines(const char *input, size_t input_length, FILE *output_file,
                          int write_results, size_t *first_error_position) {
    size_t line_start = 0;

    while (line_start < input_length) {
        size_t line_end = line_start;
        size_t first_non_blank;
        Parser parser;
        Value value;

        while (line_end < input_length && input[line_end] != '\n') {
            line_end++;
        }

        first_non_blank = line_start;
        while(first_non_blank< line_end &&
               (input[first_non_blank] == ' ' || input[first_non_blank] == '\t' ||
                input[first_non_blank] == '\r')){
            first_non_blank++;
        }

        // Blank lines and comment lines produce no output.
        if (first_non_blank <line_end&& input[first_non_blank] != '#') {
            parser.input = input;
            parser.position = line_start;
            parser.line_end = line_end;
            parser.error_position = 0;
            parser.has_error = 0;


            value = parse_expression(&parser, 0);
            skip_whitespace(&parser);

            // Any remaining character after a complete expression is the first invalid token.
            if (!parser.has_error && parser.position < parser.line_end) {
                set_error(&parser, parser.position + 1);
            }

            if (parser.has_error){


                *first_error_position = parser.error_position;

                return 0;

            }

            if (write_results) {
                if (value.is_float) {
                    if (fprintf(output_file, "%.15g\n", value.float_value) < 0) {
                        return -1;
                    }

                } else {
                    if (fprintf(output_file, "%lld\n", value.integer_value) < 0) {
                        return -1;
                    }
                }
            }
        }


        if (line_end < input_length) {
            line_start = line_end + 1;

        } else {
            line_start = input_length;


        }
    }

    return 1;
}


// Results are written only after the complete file has passed the first check.
static int evaluate_file(const char *input_path, const char *output_path){
    char *input;
    size_t input_length;
    size_t error_position = 0;
    int result;
    FILE *output_file;



    input = read_entire_file(input_path, &input_length);
    if (input == NULL) {
        fprintf(stderr, "Could not read input file: %s\n", input_path);
        return 0;
    }

    // This pass checks the complete file without writing results.
    result = evaluate_lines(input, input_length, NULL, 0, &error_position);

    output_file = fopen(output_path, "w");
    if (output_file == NULL) {
        fprintf(stderr, "Could not write output file: %s\n", output_path);
        free(input);
        return 0;}


    if (result == 0) {
        if (fprintf(output_file, "ERROR:%zu\n", error_position) < 0) {
            result = -1;

        }
    } else {
        // This pass writes results because the completefile is already known to bevalid.
        result = evaluate_lines(input, input_length, output_file, 1, &error_position);
    }



    if (fclose(output_file) != 0 || result == -1) {
        fprintf(stderr, "Could not write complete output file: %s\n", output_path);
        free(input);
        return 0;
    }

    free(input);
    return 1;
}


// Only names ending exactly in .txt are accepted in directory mode.
static int has_txt_extension(const char *file_name) {
    size_t length = strlen(file_name);
    if (length < 4) {
        return 0;
    }

    return strcmp(file_name + length - 4, ".txt") == 0;
}

// The path and .txt ending are removed to create the required output name.
static char *copy_base_name(const char *path) {
    const char *name = strrchr(path, '/');
    const char *end;
    size_t length;
    char *result;

    if (name == NULL) {
        name = path;
    } else{
        name++;

    }

    end= name + strlen(name);
    if (end - name >= 4 && strcmp(end - 4, ".txt") == 0) {
        end = end - 4;
    }

    length = (size_t)(end - name);
    result = malloc(length + 1);
    if (result == NULL) {
        return NULL;
    }
    memcpy(result, name, length);
    result[length] = '\0';
    return result;
}


// Trailing slashes are ignored when the default output folder name is created.
static char *copy_directory_name(const char *path) {
    const char *end = path + strlen(path);
    const char *start;
    size_t length;
    char *result;


    while (end > path && end[-1] == '/') {
        end--;
    }
    start = end;

    while (start > path && start[-1] != '/') {
        start--;
    }

    length =(size_t)(end - start);
    result =malloc(length + 1);
    if (result == NULL) {
        return NULL;
    }
    memcpy(result, start, length);
    result[length] = '\0';
    return result;
}

// Space for the separator and string terminator is included in the new path.
static char *combine_path(const char *folder, const char *name) {
    size_t length = strlen(folder) + strlen(name) + 2;
    char *path = malloc(length);
    if(path != NULL){
        snprintf(path, length, "%s/%s", folder, name);
    }
    return path;
}

// The output name contains the input base, student name and student ID.
static char *make_output_path(const char *output_directory, const char *input_path) {
    char *input_base = copy_base_name(input_path);
    char *file_name;
    char *output_path;
    size_t file_name_length;

    if (input_base == NULL) {
        return NULL;
    }

    file_name_length = strlen(input_base) + strlen("fabian") +
                       strlen("dehnbostel") + strlen("260AIM015") + 8;
    file_name = malloc(file_name_length);
    if (file_name == NULL) {
        free(input_base);
        return NULL;
    }

    snprintf(file_name, file_name_length, "%s_%s_%s_%s.txt",
             input_base, "fabian", "dehnbostel", "260AIM015");
    output_path = combine_path(output_directory, file_name);
    free(input_base);
    free(file_name);
    return output_path;

}



// An existing path must be a directory; otherwise mkdir creates the output directory.
static int ensure_directory_exists(const char *directory_path) {
    struct stat information;
    if (stat(directory_path, &information) == 0){
        return S_ISDIR(information.st_mode);
    }

    if (mkdir(directory_path, 0755) == 0) {
        return 1;
    }

    return 0;

}



// The output path is created before the input file is evaluated.
static int process_one_file(const char *input_path, const char *output_directory) {
    char *output_path = make_output_path(output_directory, input_path);
    int success;

    if(output_path == NULL){
        fprintf(stderr, "Not enough memory.\n");

        return 0;
    }


    success = evaluate_file(input_path, output_path);
    free(output_path);
    return success;
}


static int process_directory(const char *input_directory, const char *output_directory){

    DIR *directory = opendir(input_directory);
    struct dirent *entry;


    if (directory == NULL) {
        fprintf(stderr, "Could not open input directory: %s\n", input_directory);
        return 0;
    }



    while ((entry = readdir(directory)) != NULL) {
        char *input_path;
        struct stat information;

        if (!has_txt_extension(entry->d_name)) {
            continue;
        }
        input_path = combine_path(input_directory, entry->d_name);
        if (input_path == NULL) {
            closedir(directory);
            fprintf(stderr, "Not enough memory.\n");
            return 0;
        }
        if (stat(input_path, &information) == 0 && S_ISREG(information.st_mode)) {
            if (!process_one_file(input_path, output_directory)) {
                free(input_path);
                closedir(directory);

                return 0;
            }
        }

        free(input_path);

    }

    closedir(directory);
    return 1;

}



// The command line must select exactly one input file or one input directory.
int main(int argument_count, char *argument_values[]) {
    const char *input_file = NULL;
    const char *input_directory = NULL;
    const char *output_directory = NULL;
    char *default_output_directory = NULL;
    int argument_index;
    int success;

    for (argument_index = 1; argument_index < argument_count; argument_index++){
        if (strcmp(argument_values[argument_index], "-d") == 0 ||
            strcmp(argument_values[argument_index], "--dir") == 0) {
            if (argument_index + 1 >= argument_count || input_directory != NULL) {
                fprintf(stderr, "Invalid directory option.\n");
                return 1;
            }


            argument_index++;
            input_directory = argument_values[argument_index];


        } else if(strcmp(argument_values[argument_index], "-o") == 0 ||
                   strcmp(argument_values[argument_index], "--output-dir") == 0) {
            if (argument_index + 1 >= argument_count || output_directory != NULL) {
                fprintf(stderr, "Invalid output option.\n");
                return 1;
            }


            argument_index++;
            output_directory = argument_values[argument_index];
        } else if (argument_values[argument_index][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argument_values[argument_index]);
            return 1;


        } else if (input_file == NULL) {
            input_file = argument_values[argument_index];


        } else {
            fprintf(stderr, "Too many input files.\n");
            return 1;


        }

    }

    if((input_file == NULL && input_directory == NULL) ||
        (input_file != NULL && input_directory != NULL)){
        fprintf(stderr, "Use one input file or one input directory.\n");

        return 1;
    }

    if (output_directory == NULL) {
        char *input_base;
        size_t length;

        if (input_file != NULL) {
            input_base = copy_base_name(input_file);
        } else {
            input_base = copy_directory_name(input_directory);
        }

        if (input_base == NULL) {
            fprintf(stderr, "Not enough memory.\n");
            return 1;
        }

        length = strlen(input_base) + strlen("fabian") + strlen("260AIM015") + 3;
        default_output_directory = malloc(length);
        if (default_output_directory == NULL) {
            free(input_base);
            fprintf(stderr, "Not enough memory.\n");
            return 1;
        }

        snprintf(default_output_directory, length, "%s_%s_%s",
                 input_base, "fabian", "260AIM015");
        free(input_base);
        output_directory = default_output_directory;
    }

    if (!ensure_directory_exists(output_directory)) {
        fprintf(stderr, "Could not create or use output directory: %s\n", output_directory);
        free(default_output_directory);
        return 1;

    }

    if(input_file != NULL){
        success = process_one_file(input_file, output_directory);
    }else{
        success = process_directory(input_directory, output_directory);
    }


    free(default_output_directory);
    if(!success){


        return 1;

    }
    return 0;
    
}