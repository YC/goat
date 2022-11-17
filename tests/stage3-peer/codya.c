// A (hopefully faithful) reproduction of codya.gt in C

#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

void explore();
void get_info(bool*, int*, bool*);
void get_edge(int*, int*, bool);
void print_type(bool isDFS);
void advance(bool isDFS, int* start, int* qend);
void choose(bool isDFS, int* start, int* qend, int* ret);

// ###################################
// # Simple BFS+DFS graph explorer   #
// # by Cody Anderson                #
// ###################################

int main() {
    explore();
    return 0;
}

void explore() {
    //Node count
    int node_count = 0;

    //Used for input
    int from = 0;
    int to = 0;

    //Turn off input
    bool echo = false;

    //For input, determins if DFS or BFS
    bool isDFS = false;

    //Iter variable
    int i = 0;

    //Cur ndoe
    int cur = 0;

    //For returning values from choose
    int ret = 0;

    //Track visited edges
    bool visited[100] = {false};

    // Graph itself, using a adjacency matrix
    // not the best DS for this job but it is easy
    bool edges[100][100];
    for (int i = 0; i < 100; i++) {
        for (int j = 0; j < 100; j++) {
            edges[i][j] = false;
        }
    }

    //Queue data structure (possibly isnt large enough?)
    int queue[1000] = {0};
    int qend = 0;
    int start = 0;

    // Handle all the input and construct the graph
    get_info(&isDFS, &node_count, &echo);
    if (echo) {
        printf("\nAdd some edges (-1 to stop)\n");
    }
    get_edge(&from, &to, echo);
    while (from != -1 && to != -1) {
        edges[from][to] = true;
        get_edge(&from, &to, echo);
    }

    // Push all the edges of the first node to the queue
    // this code is repeated later unfortunatly, but due to
    // goats limitations not a lot of choices
    visited[0] = true;
    while (i < node_count) {
        if (edges[0][i] && !visited[i]) {
            queue[qend] = i;
            qend = qend + 1;
        }
        i = i + 1;
    }

    // Print the first node and set up the next one
    printf("%d", 0);
    choose(isDFS, &start, &qend, &ret);
    cur = queue[ret];

    while (start != qend) {
        printf(" -> ");

        // Set this node as visited and remove it from the queue
        visited[cur] = true;
        printf("%d", cur);
        advance(isDFS, &start, &qend);

        // Repeated code, push edges to queue
        i = 0;
        while (i < node_count) {
            if (!visited[i] && edges[cur][i]) {
                queue[qend] = i;
                qend = qend + 1;
            }
            i = i + 1;
        }

        // Find the next node to vist, skipping over any alredy visited ones
        choose(isDFS, &start, &qend, &ret);
        // assert(ret >= 0); // This fails!!!
        cur = queue[ret];
        while (visited[cur] && start != qend) {
            advance(isDFS, &start, &qend);
            choose(isDFS, &start, &qend, &ret);
            cur = queue[ret];
        }
    }
    printf("\n");
}

// Get all the basic info (size and DFS or BFS) and print some
// info for the users benefit
void get_info(bool* isDFS, int* node_count, bool* echo) {
    printf("Print? (true or false)\n");

    char buf[6];
    char t[5] = "true";

    // A naive implementation of "read echo;"
    scanf("%s", (char*) &buf);
    if (strncmp((char*) &buf, (char*) &t, 4) == 0) {
        *echo = true;
    }

    if (*echo) {
        printf("DFS (true) or BFS (false)?:\n");
    }

    // A naive implementation of "read isDFS;"
    scanf("%s", (char*) &buf);
    if (strncmp((char*) &buf, (char*) &t, 4) == 0) {
        *isDFS = true;
    }

    if (*echo) {
        printf("How many Nodes (< 101):\n");
        printf(":> ");
    }

    scanf("%d", node_count);
    if (*echo) {
        printf("Your nodes are labeled 0 to ");
        printf("%d", *node_count-1);
        printf(" (0 is the start node)\n");
    }
}

// Get an edge from the user
void get_edge(int* from, int* to, bool echo) {
    if (echo) {
        printf("From: ");
    }
    scanf("%d", from);
    if (*from != -1) {
        if (echo) {
            printf("To: ");
        }
        scanf("%d", to);
    }
}


//These 3 procs do the BFS or DFS specific behaviour
//Print a DFS or BFS label just before the output
void print_type(bool isDFS) {
    if (isDFS) {
        printf("\nDFS: ");
    } else {
        printf("\nBFS: ");
    }
}

// Move on to the next edge to explore, goes forward or backward
// depending on the type of search
void advance(bool isDFS, int* start, int* qend) {
    if (isDFS) {
        *qend = *qend - 1;
    } else {
        *start = *start + 1;
    }
}

// Choose to look at the start or the end of the queue
// (depending on what type of search) for the next value
void choose(bool isDFS, int* start, int* qend, int* ret) {
    if (isDFS) {
        *ret = *qend-1;
    } else {
        *ret = *start;
    }
}
