#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINES (1024)*5
#define MAX_LINE_LEN 1024

void read_lines(const char *file_path, int *line_count, char buffer[][MAX_LINE_LEN]) {
    FILE *fp;
    fp = fopen(file_path, "r");

    while (!feof(fp) && !ferror(fp))
        if (fgets(buffer[*line_count], MAX_LINE_LEN, fp) != NULL)
            (*line_count)++;

    fclose(fp);
}

typedef struct {
    int x;
    int y;
    int z;
} Triangle;

void parse_triangle(char line[MAX_LINE_LEN], Triangle *t) {
    sscanf(line, "%d %d %d", &t->x, &t->y, &t->z);
}

bool valid_triangle(int x, int y, int z) {
    return x + y > z && y + z > x && z + x > y;
}

int part_1(int line_count, char lines[][MAX_LINE_LEN]) {
    int valid_triangles = 0;
    for (int i = 0; i < line_count; i++) {
        Triangle t = {0};
        parse_triangle(lines[i], &t);
        valid_triangle(t.x, t.y, t.z) && valid_triangles++;
    }
    return valid_triangles;
}

int part_2(int line_count, char lines[][MAX_LINE_LEN]) {
    int valid_triangles = 0;
    for (int i = 0; i < line_count; i += 3) {
        Triangle t0, t1, t2 = {0};
        parse_triangle(lines[i], &t0);
        parse_triangle(lines[i + 1], &t1);
        parse_triangle(lines[i + 2], &t2);
        valid_triangles += valid_triangle(t0.x, t1.x, t2.x) +
                           valid_triangle(t0.y, t1.y, t2.y) +
                           valid_triangle(t0.z, t1.z, t2.z);
    }
    return valid_triangles;
}

void solve_file(const char *file_path) {
    printf("Solving for file : %s\n", file_path);

    // Here we ares uper lazy and just read file lines to constant size array
    // with arbitrary bounds.
    char lines[MAX_LINES][MAX_LINE_LEN];
    int line_count = 0;

    read_lines(file_path, &line_count, lines);

    printf("part 1 : %d\n", part_1(line_count, lines));
    printf("part 2 : %d\n", part_2(line_count, lines));
}

int main(int argc, char **argv) {
    for (int i = 1; i < argc; i++)
        solve_file(argv[i]);

    return 0;
}
