#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>
#include <openacc.h>
#include <jacc.h>
#include <unistd.h>
#include <ffi.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

/* https://github.com/mirek/rb_tree */
#define RB_ITER_MAX_HEIGHT 64
struct rb_node;struct rb_tree;typedef int (*rb_tree_node_cmp_f)(struct rb_tree *self,struct rb_node *a,struct rb_node *b);typedef void (*rb_tree_node_f)(struct rb_tree *self,struct rb_node *node);struct rb_node{int red;struct rb_node *link[2];void *value;};struct rb_tree{struct rb_node *root;rb_tree_node_cmp_f cmp;size_t size;void *info;};struct rb_iter{struct rb_tree *tree;struct rb_node *node;struct rb_node *path[RB_ITER_MAX_HEIGHT];size_t top;void *info;};int rb_tree_node_cmp_ptr_cb(struct rb_tree *self,struct rb_node *a,struct rb_node *b);void rb_tree_node_dealloc_cb(struct rb_tree *self,struct rb_node *node);struct rb_node *rb_node_alloc();struct rb_node *rb_node_create(void *value);struct rb_node *rb_node_init(struct rb_node *self, void *value);void rb_node_dealloc(struct rb_node *self);struct rb_tree *rb_tree_alloc();struct rb_tree *rb_tree_create(rb_tree_node_cmp_f cmp);struct rb_tree *rb_tree_init(struct rb_tree *self,rb_tree_node_cmp_f cmp);void rb_tree_dealloc(struct rb_tree *self,rb_tree_node_f node_cb);void *rb_tree_find(struct rb_tree *self,void *value);int rb_tree_insert(struct rb_tree *self,void *value);int rb_tree_remove(struct rb_tree *self,void *value);size_t rb_tree_size(struct rb_tree *self);int rb_tree_insert_node(struct rb_tree *self,struct rb_node *node);int rb_tree_remove_with_cb(struct rb_tree *self,void *value,rb_tree_node_f node_cb);int rb_tree_test(struct rb_tree *self,struct rb_node *root);struct rb_iter *rb_iter_alloc();struct rb_iter *rb_iter_init(struct rb_iter *self);struct rb_iter *rb_iter_create();void rb_iter_dealloc(struct rb_iter *self);void *rb_iter_first(struct rb_iter *self,struct rb_tree *tree);void *rb_iter_last(struct rb_iter *self,struct rb_tree *tree);void *rb_iter_next(struct rb_iter *self);void *rb_iter_prev(struct rb_iter *self);struct rb_node *rb_node_alloc(){return malloc(sizeof(struct rb_node));}struct rb_node *rb_node_init(struct rb_node *self,void *value){if(self){self->red=1;self->link[0]=self->link[1]=NULL;self->value=value;}return self;}struct rb_node *rb_node_create(void *value){return rb_node_init(rb_node_alloc(),value);}void rb_node_dealloc(struct rb_node *self){if(self){free(self);}}static int rb_node_is_red(const struct rb_node *self){return self?self->red:0;}static struct rb_node *rb_node_rotate(struct rb_node *self,int dir){struct rb_node *result=NULL;if(self){result=self->link[!dir];self->link[!dir]=result->link[dir];result->link[dir]=self;self->red=1;result->red=0;}return result;}static struct rb_node *rb_node_rotate2(struct rb_node *self,int dir){struct rb_node *result=NULL;if(self){self->link[!dir]=rb_node_rotate(self->link[!dir],!dir);result=rb_node_rotate(self,dir);}return result;}int rb_tree_node_cmp_ptr_cb(struct rb_tree *self,struct rb_node *a,struct rb_node *b){return(a->value>b->value)-(a->value<b->value);}void rb_tree_node_dealloc_cb(struct rb_tree *self,struct rb_node *node){if(self){if(node){rb_node_dealloc(node);}}}struct rb_tree *rb_tree_alloc(){return malloc(sizeof(struct rb_tree));}struct rb_tree *rb_tree_init(struct rb_tree *self,rb_tree_node_cmp_f node_cmp_cb){if(self){self->root=NULL;self->size=0;self->cmp=node_cmp_cb?node_cmp_cb:rb_tree_node_cmp_ptr_cb;}return self;}struct rb_tree *rb_tree_create(rb_tree_node_cmp_f node_cb){return rb_tree_init(rb_tree_alloc(),node_cb);}void rb_tree_dealloc(struct rb_tree *self,rb_tree_node_f node_cb){if(self){if(node_cb){struct rb_node *node=self->root;struct rb_node *save=NULL;while(node){if(node->link[0]==NULL){save=node->link[1];node_cb(self,node);node=NULL;}else{save=node->link[0];node->link[0]=save->link[1];save->link[1]=node;}node=save;}}free(self);}}int rb_tree_test(struct rb_tree *self,struct rb_node *root){int lh,rh;if(root==NULL)return 1;else{struct rb_node *ln=root->link[0];struct rb_node *rn=root->link[1];if(rb_node_is_red(root)){if(rb_node_is_red(ln)||rb_node_is_red(rn)){printf("Red violation");return 0;}}lh=rb_tree_test(self,ln);rh=rb_tree_test(self,rn);if((ln!=NULL&&self->cmp(self,ln,root)>=0)||(rn!=NULL&&self->cmp(self,rn,root)<=0)){puts("Binary tree violation");return 0;}if(lh!=0&&rh!=0&&lh!=rh){puts("Black violation");return 0;}if(lh!=0&&rh!=0) return rb_node_is_red(root)?lh:lh+1;else return 0;}}void *rb_tree_find(struct rb_tree *self,void *value){void *result=NULL;if(self){struct rb_node node={.value=value};struct rb_node *it=self->root;int cmp=0;while(it){if((cmp=self->cmp(self,it,&node))){it=it->link[cmp<0];}else{break;}}result=it?it->value:NULL;}return result;}int rb_tree_insert(struct rb_tree *self,void *value){return rb_tree_insert_node(self,rb_node_create(value));}int rb_tree_insert_node(struct rb_tree *self,struct rb_node *node){int result=0;if(self&&node){if(self->root==NULL){self->root=node;result=1;}else{struct rb_node head={0};struct rb_node *g,*t;struct rb_node *p,*q;int dir=0,last=0;t=&head;g=p=NULL;q=t->link[1]=self->root;while(1){if(q==NULL){p->link[dir]=q=node;}else if(rb_node_is_red(q->link[0])&&rb_node_is_red(q->link[1])){q->red=1;q->link[0]->red=0;q->link[1]->red=0;}if(rb_node_is_red(q)&&rb_node_is_red(p)){int dir2=t->link[1]==g;if(q==p->link[last]){t->link[dir2]=rb_node_rotate(g,!last);}else{t->link[dir2]=rb_node_rotate2(g,!last);}}if(self->cmp(self,q,node)==0){break;}last=dir;dir=self->cmp(self,q,node)<0;if(g!=NULL){t=g;}g=p,p=q;q=q->link[dir];}self->root=head.link[1];}self->root->red=0;++self->size;}return 1;}int rb_tree_remove_with_cb(struct rb_tree *self,void *value,rb_tree_node_f node_cb){if(self->root!=NULL){struct rb_node head={0};struct rb_node node={.value=value};struct rb_node *q,*p,*g;struct rb_node *f=NULL;int dir=1;q=&head;g=p=NULL;q->link[1]=self->root;while(q->link[dir]!=NULL){int last=dir;g=p,p=q;q=q->link[dir];dir=self->cmp(self,q,&node)<0;if(self->cmp(self,q,&node)==0){f=q;}if(!rb_node_is_red(q)&&!rb_node_is_red(q->link[dir])){if(rb_node_is_red(q->link[!dir])){p=p->link[last]=rb_node_rotate(q,dir);}else if(!rb_node_is_red(q->link[!dir])){struct rb_node *s=p->link[!last];if(s){if(!rb_node_is_red(s->link[!last])&&!rb_node_is_red(s->link[last])){p->red=0;s->red=1;q->red=1;}else{int dir2=g->link[1]==p;if(rb_node_is_red(s->link[last])){g->link[dir2]=rb_node_rotate2(p,last);}else if(rb_node_is_red(s->link[!last])){g->link[dir2]=rb_node_rotate(p,last);}q->red=g->link[dir2]->red=1;g->link[dir2]->link[0]->red=0;g->link[dir2]->link[1]->red=0;}}}}}if(f){void *tmp=f->value;f->value=q->value;q->value=tmp;p->link[p->link[1]==q]=q->link[q->link[0]==NULL];if(node_cb){node_cb(self,q);}q=NULL;}self->root=head.link[1];if(self->root!=NULL){self->root->red=0;}--self->size;}return 1;}int rb_tree_remove(struct rb_tree *self,void *value){int result=0;if(self){result=rb_tree_remove_with_cb(self,value,rb_tree_node_dealloc_cb);}return result;}size_t rb_tree_size(struct rb_tree *self){size_t result=0;if(self){result=self->size;}return result;}struct rb_iter *rb_iter_alloc(){return malloc(sizeof(struct rb_iter));}struct rb_iter *rb_iter_init(struct rb_iter *self){if(self){self->tree=NULL;self->node=NULL;self->top=0;}return self;}struct rb_iter *rb_iter_create(){return rb_iter_init(rb_iter_alloc());}void rb_iter_dealloc(struct rb_iter *self){if(self){free(self);}}static void *rb_iter_start(struct rb_iter *self,struct rb_tree *tree,int dir){void *result=NULL;if(self){self->tree=tree;self->node=tree->root;self->top=0;if(self->node!=NULL){while(self->node->link[dir]!=NULL){self->path[self->top++]=self->node;self->node=self->node->link[dir];}}result=self->node==NULL?NULL:self->node->value;}return result;}static void *rb_iter_move(struct rb_iter *self,int dir){if(self->node->link[dir]!=NULL){self->path[self->top++]=self->node;self->node=self->node->link[dir];while(self->node->link[!dir]!=NULL){self->path[self->top++]=self->node;self->node=self->node->link[!dir];}}else{struct rb_node *last=NULL;do{if(self->top==0){self->node=NULL;break;}last=self->node;self->node=self->path[--self->top];}while(last==self->node->link[dir]);}return self->node==NULL?NULL:self->node->value;}void *rb_iter_first(struct rb_iter *self,struct rb_tree *tree){return rb_iter_start(self,tree,0);}void *rb_iter_last(struct rb_iter *self,struct rb_tree *tree){return rb_iter_start(self,tree,1);}void *rb_iter_next(struct rb_iter *self){return rb_iter_move(self,1);}void *rb_iter_prev(struct rb_iter *self){return rb_iter_move(self,0);}

#ifdef DEBUG
#undef DEBUG
#define DEBUG(msg, ...) fprintf(stdout, "[JACC] " msg "\n", ##__VA_ARGS__);
#else
#define DEBUG(msg, ...) do {} while (0)
#endif

#ifdef __PGIC__
#undef __GNUC__
#endif

#if !defined(JACC_OPENACC) || !defined(JACC_OPENACC_OPTION)
#ifdef __GNUC__
#define JACC_OPENACC gcc
#define JACC_OPENACC_OPTION -fopenacc -foffload=nvptx-none -O3 -foffload=-lm
#else
#define JACC_OPENACC pgcc
#define JACC_OPENACC_OPTION -acc -ta=tesla:cc70 -O3 -Mcuda
#endif
#endif

#ifdef __GNUC__
void acc_set_default_async(int async)
{
/* The default_async doesn't work on GCC */
//#pragma acc set default_async(async)
}
#endif

#define XSTR(s) #s
#define STR(s) XSTR(s)

#define JaccArg __JaccArg
#define jacc_kernel_push __jacc_kernel_push
#define jacc_copyin __jacc_copyin
#define jacc_create __jacc_create
#define jacc_copyout __jacc_copyout
#define jacc_delete __jacc_delete
#define jacc_update_self __jacc_update_self
#define jacc_update_device __jacc_update_device
#define jacc_init __jacc_init
#define jacc_close __jacc_close
#define jacc_wait __jacc_wait
#define jacc_acc_init __jacc_acc_init
#define jacc_acc_shutdown __jacc_acc_shutdown
#define jacc_optimize __jacc_optimize
#define JACC_ARRAY __JACC_ARRAY
#define JACC_STATIC __JACC_STATIC
#define JACC_PRESENT __JACC_PRESENT
#define JACC_REDUCTED __JACC_REDUCTED
#define JACC_WRITTEN __JACC_WRITTEN

static bool OPTIMIZED;
static size_t JACC_ASYNC_STAMP;

static char JACC_TEMPDIR[0xff];

#define JACC_TABLE_SIZE 32768
#define TABLE_INDEX(key) (((long)key / 16) % JACC_TABLE_SIZE)

struct JaccTableEntry {
    char *key;
    char *label; // N => naive, O => Optimized

    void *kernel;
    size_t nargs;
    ffi_cif cif;
    ffi_type **types;
    void **values;

    bool *restrictable;

    bool *fixed;

    struct JaccArg *arg;

    size_t id; // For the purpose of debugging
    struct JaccTableEntry *next;
};

struct JaccTableEntry *JACC_TABLE[JACC_TABLE_SIZE];

typedef struct async_list {
    int n;
    unsigned long timestamp;
    struct async_list *next;
} async_list;

typedef struct JaccPresent {
    void *addr;
    size_t len;
    int async_write;
    async_list *async_read;
    unsigned long timestamp;
} JaccPresent;

static unsigned long JACC_TIMESTAMP = 0;

typedef struct rb_tree rb_tree;
typedef struct rb_node rb_node;

int
rbcmp(rb_tree *self, rb_node *node_a, rb_node *node_b)
{
    JaccPresent *a = (JaccPresent*)node_a->value;
    JaccPresent *b = (JaccPresent*)node_b->value;

    if ((a->addr <= b->addr && a->addr + a->len - 1 >= b->addr) ||
        (b->addr <= a->addr && b->addr + b->len - 1 >= a->addr))
        return 0;

    return (a->addr > b->addr) - (a->addr < b->addr);
}

rb_tree *JACC_PRESENT_TREE = NULL;
static int JACC_ASYNC_NUM = 0;

// Async switch
#define JACC_MAX_ASYNC 16
#define JACC_ASYNC_INC JACC_ASYNC_NUM=(JACC_ASYNC_NUM+1)%JACC_MAX_ASYNC
//#define JACC_ASYNC_INC JACC_ASYNC_NUM=JACC_ASYNC_NUM

static unsigned long JACC_ASYNC_SOLVED[JACC_MAX_ASYNC][JACC_MAX_ASYNC] = { 0UL };
static bool JACC_ASYNC_OUT[JACC_MAX_ASYNC] = { false };

struct JaccTableEntry *
jacc_table_manipulate(const char *key, char *label, bool delete)
{
    size_t index = TABLE_INDEX(key);
    struct JaccTableEntry *previous = NULL, *entry = JACC_TABLE[index];

    while (entry != NULL) {
        if (entry->key == key && !strcmp(entry->label, label)) {
            // Move to the first position
            if (previous != NULL) {
                previous->next = entry->next;
                entry->next = JACC_TABLE[index];
                if (!delete)
                    JACC_TABLE[index] = entry;
            }
            else if (delete) {
                JACC_TABLE[index] = JACC_TABLE[index]->next;
            }

            return entry;
        }

        previous = entry;
        entry = entry->next;
    }

    return NULL;
}

struct JaccTableEntry *
jacc_table_find(const char *key, char *label)
{
    return jacc_table_manipulate(key, label, false);
}

struct JaccTableEntry *
jacc_table_delete(char *key, char *label)
{
    return jacc_table_manipulate(key, label, true);
}

JaccArg *
jacc_arg_copy(JaccArg *arg)
{
    if (arg == NULL) return NULL;

    JaccArg *ret = (JaccArg *)malloc(sizeof(JaccArg));
    JaccArg *next = jacc_arg_copy(arg->next);

    ret = memcpy(ret, arg, sizeof(JaccArg));
    ret->data = malloc(arg->size);
    ret->data = memcpy(ret->data, arg->data, arg->size);
    ret->next = next;
    return ret;
}

void
jacc_arg_free(JaccArg *arg)
{
    if (arg == NULL) return;

    JaccArg *next = arg->next;

    free(arg->data);
    free(arg);
    jacc_arg_free(next);
}

struct JaccTableEntry *
jacc_table_insert(const char *code, char *label, void *kernel, size_t nargs,
                  ffi_cif cif, ffi_type **types, void **values,
                  struct JaccArg *arg, size_t id)
{
    struct JaccTableEntry *entry = malloc(sizeof(struct JaccTableEntry));
    size_t index = TABLE_INDEX(code);

    entry->key = (void*)code;
    entry->label = label;
    entry->kernel = kernel;

    entry->nargs = nargs;
    entry->cif = cif;
    entry->types = types;
    entry->values = values;

    entry->restrictable = (bool*)malloc(sizeof(bool) * nargs);
    for (int i = 0; i < nargs; i++)
        entry->restrictable[i] = true;

    entry->fixed = (bool*)malloc(sizeof(bool) * nargs);
    for (int i = 0; i < nargs; i++)
        entry->fixed[i] = true;

    entry->arg = jacc_arg_copy(arg);

    entry->id = id;
    entry->next = JACC_TABLE[index];
    JACC_TABLE[index] = entry;

    return entry;
}

void
jacc_table_entry_free(struct JaccTableEntry *entry)
{
    if (entry != NULL) {
        struct JaccTableEntry *next = entry->next;

        free(entry->types);
        free(entry->values);
        jacc_arg_free(entry->arg);

        free(entry);
        jacc_table_entry_free(next);
    }
}

void
jacc_arg_print_data(FILE *fp, JaccArg *arg)
{
    if (arg->attr & JACC_ARRAY) {
        fprintf(fp, "(%s)%p", arg->type, *(void**)arg->data);
    }
    else if (strcmp(arg->type, "int") == 0)
        fprintf(fp, "%d", *(int*)(arg->data));
    else if (strcmp(arg->type, "float") == 0)
        fprintf(fp, "%.30e", *(float*)(arg->data));
    else if (strcmp(arg->type, "double") == 0)
        fprintf(fp, "%.60le", *(double*)(arg->data));
    else if (strcmp(arg->type, "unsigned") == 0)
        fprintf(fp, "%uu", *(unsigned*)(arg->data));
    else {
        fprintf(stderr, "[JACC] Unknown type: %s of %s\n",
                arg->type, arg->symbol);
        exit(-1);
    }
}

static
ffi_type *specifier_to_ffi_type(const char *spec)
{
    if (strstr(spec, "*"))
        return &ffi_type_pointer;
    if (!strcmp(spec, "int"))
        return &ffi_type_sint;
    if (!strcmp(spec, "float"))
        return &ffi_type_float;
    if (strcmp(spec, "double") == 0)
        return &ffi_type_double;
    if (strcmp(spec, "unsigned") == 0)
        return &ffi_type_uint;

    fprintf(stderr, "[JACC] Could not convert to ffi_type: %s\n",
            spec);
    exit(1);

    return &ffi_type_void;
}

void
jacc_arg_print_decl(FILE *fp, JaccArg *arg, bool pointer)
{
    if (arg->attr & JACC_ARRAY) {
        for (int i = 0, c; c = arg->type[i]; i++) {
            fprintf(fp, "%c", c);

            if (c == '*') {
                if (pointer) fprintf(fp, "*");
                fprintf(fp, "%s", arg->symbol);
                if (pointer) fprintf(fp, "__ptr");
            }
        }
    } else {
        fprintf(fp, "%s ", arg->type);
        if (pointer) fprintf(fp, "*");
        fprintf(fp, "%s", arg->symbol);
        if (pointer) fprintf(fp, "__ptr");
    }
}

void
jacc_arg_print_decl_restrict(FILE *fp, JaccArg *arg, bool pointer)
{
    if (arg->attr & JACC_ARRAY) {
        for (int i = 0, c; c = arg->type[i]; i++) {
            fprintf(fp, "%c", c);

            if (c == '*') {
                if (pointer) fprintf(fp, "*");
                fprintf(fp, " restrict %s", arg->symbol);
                if (pointer) fprintf(fp, "__ptr");
            }
        }
    } else {
        fprintf(fp, "%s ", arg->type);
        if (pointer) fprintf(fp, "*");
        fprintf(fp, "%s", arg->symbol);
        if (pointer) fprintf(fp, "__ptr");
    }
}

bool is_dereferenced(JaccArg *arg)
{
    return (!(arg->attr & JACC_ARRAY) &&
            arg->attr & JACC_WRITTEN &&
            ((arg->attr & JACC_PRESENT) ||
             ((arg->attr & JACC_REDUCTED))));
}

struct JaccTableEntry *
jacc_jit(const char *kernel_code, JaccArg *kernel_arg)
{
    static int kernel_count = 0; kernel_count += 1;

    if (strncmp(JACC_TEMPDIR, "/tmp/jacc.", 10) != 0) {
        jacc_init();
    }

    char code_path[0xff], lib_path[0xff], command[0xff], kernel_name[0xff];

    sprintf(code_path, "%s/%d.c", JACC_TEMPDIR, kernel_count);
    sprintf(lib_path, "%s/%d.so", JACC_TEMPDIR, kernel_count);
    sprintf(command,
            STR(JACC_OPENACC) " "
            STR(JACC_OPENACC_OPTION) " -shared -o %s %s",
            lib_path, code_path);
    sprintf(kernel_name, "jacc_jit_kernel%d", kernel_count);

    FILE *fp;

    if ((fp = fopen(code_path, "w")) == NULL) {
        perror("[JACC] Error on fopen");
        exit(-1);
    }

    int nargs = 0;
    ffi_cif cif;
    ffi_type **types;
    void **values;

    for (JaccArg *arg = kernel_arg; arg != NULL; arg = arg->next)
        nargs++;

    // jacc_async
    nargs++;

    types  = (ffi_type **)calloc(nargs, sizeof(ffi_type *));
    values = (void **)calloc(nargs, sizeof(void *));

    // Args
    // - pointer used for array or var_write presented/reducted
    // - variable used for other variables
    fprintf(fp, "void %s (", kernel_name);

    int i = 0;
    for (JaccArg *arg = kernel_arg; arg != NULL; arg = arg->next) {
        if (i != 0)
            fprintf(fp, ", ");

        bool dereference = is_dereferenced(arg);

        jacc_arg_print_decl(fp, arg, dereference);

        if ((arg->attr & JACC_ARRAY) || dereference) {
            types[i] = &ffi_type_pointer;
        }
        else {
            types[i] = specifier_to_ffi_type(arg->type);
        }

        i++;
    }

    // jacc_async
    fprintf(fp, ", int jacc_async");
    types[i] = specifier_to_ffi_type("int");

    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nargs,
                     &ffi_type_void, types) != FFI_OK) {
        perror("[JACC] Error on ffi_prep_cif");
        exit(-1);
    }

    fprintf(fp, ")\n{\n");

    // Constant variables
    for (JaccArg *arg = kernel_arg; arg != NULL; arg = arg->next) {
        if (is_dereferenced(arg)) {
            jacc_arg_print_decl(fp, arg, false);

            fprintf(fp, " = ");

            fprintf(fp, "(*(%s__ptr))", arg->symbol);

            fprintf(fp, ";\n");
        }
    }

    fputs("#define async async(jacc_async)\n", fp);
#ifdef __GNUC__
    fputs("#define num_workers(a)\n", fp);
#endif

    fputs(kernel_code, fp);

    // Assign to the address to which pointers indicate.
    // Only for var_write presented/reducted.
    for (JaccArg *arg = kernel_arg; arg != NULL; arg = arg->next) {
        if (is_dereferenced(arg)) {
            fprintf(fp, "(*(%s__ptr)) = %s;\n", arg->symbol, arg->symbol);
        }
    }

    fprintf(fp, "\n}", kernel_name);

    fclose(fp);

    system(command);

    void *handle, *kernel;
    char *error;

    if (!(handle = dlopen(lib_path, RTLD_NOW))) {
        fprintf(stderr, "[JACC] Error on dlopen: %s\n", dlerror());
        exit(-1);
    }

    kernel = dlsym(handle, kernel_name);

    if ((error = dlerror()) != NULL) {
        fprintf(stderr, "[JACC] Error on dlsym: %s\n", error);
        exit(-1);
    }

    return jacc_table_insert(kernel_code, "N", kernel,
                             nargs, cif, types, values, kernel_arg,
                             kernel_count);
}

JaccPresent *
jacc_lookup_present(void *a)
{
    return rb_tree_find(JACC_PRESENT_TREE,
                        & (JaccPresent) { .addr = a, .len = 1 });
}

// Return the header address of present data
void *
jacc_data_exist(void *a)
{
    JaccPresent *p = jacc_lookup_present(a);

    if (p) return p->addr;

    return NULL;
}

void
jacc_perform_kernel(const char *kernel_code, JaccArg *kernel_arg, int async)
{
    struct JaccTableEntry *entry = jacc_table_find(kernel_code, "N");

    entry = (entry != NULL) ? entry : jacc_jit(kernel_code, kernel_arg);

    int i = 0;
    for (JaccArg *a1 = kernel_arg; a1 != NULL; a1 = a1->next, i++) {
        if (entry->restrictable[i]) {
            for (JaccArg *a2 = a1->next; a2 != NULL; a2 = a2->next) {
                if ((a1->attr & JACC_ARRAY) && (a2->attr & JACC_ARRAY)) {
                    void *ha1 = jacc_data_exist(a1->addr);
                    void *ha2 = jacc_data_exist(a2->addr);

                    if (ha1 != NULL && ha1 == ha2)
                        entry->restrictable[i] = false;
                }
            }
        }
    }

    i = 0;
    for (JaccArg *arg = kernel_arg, *a2 = entry->arg;
         arg != NULL; arg = arg->next, a2 = a2->next, i++) {
        if (entry->fixed[i]) {
            if (arg->attr & JACC_ARRAY ||
                arg->attr & JACC_WRITTEN ||
                memcmp(arg->data, a2->data, arg->size)) {
                entry->fixed[i] = false;
            }
        }

        entry->values[i] = is_dereferenced(arg) ? &(arg->addr) : arg->data;
    }

    entry->values[i] = &async;

    int kernel_result;
    ffi_call(&(entry->cif), entry->kernel, &kernel_result, entry->values);
}

void
jacc_perform_kernel_optimized(
    const char *kernel_code, JaccArg *kernel_arg, int async)
{
    struct JaccTableEntry *entry = jacc_table_find(kernel_code, "O");

    if (entry == NULL) {
        fprintf(stderr, "[JACC] Unknown kernel: %s\n", kernel_code);
        exit(1);
    }

    int i = 0, j = 0;
    for (JaccArg *arg = kernel_arg; arg != NULL; arg = arg->next, i++) {
        if (entry->fixed[i]) continue;

        entry->values[j] = is_dereferenced(arg) ? &(arg->addr) : arg->data;
        j++;
    }

    entry->values[j] = &async;

    int kernel_result;
    ffi_call(&(entry->cif), entry->kernel, &kernel_result, entry->values);
}

void
jacc_kernel_push(const char *kernel_code, JaccArg *kernel_arg)
{
    #define JACC_MAX_ARGS 100
    JaccPresent *ps[JACC_MAX_ARGS];

    bool wait = false;
    async_list *deps = NULL;
    int async = -1;

    JACC_TIMESTAMP++;

    int i = 0;
    for (JaccArg *arg = kernel_arg; arg != NULL; arg = arg->next, i++) {
        bool pa = ((arg->attr & JACC_ARRAY) && (arg->attr & JACC_PRESENT));

        ps[i] = pa ? jacc_lookup_present(arg->addr) : NULL;

        if (pa && !ps[i]) {
            fprintf(stderr, "[JACC] Not present: %s\n", arg->symbol);
            exit(-1);
        }

        // Fix reference to static array
        if (arg->attr & JACC_STATIC) {
            arg->data = &(arg->addr);
        }

        if (ps[i]) {
            async_list *x;

            /* Update deps */
            if (arg->attr & JACC_WRITTEN) {
                // Wait & remove async_read
                if (ps[i]->async_read) {
                    for (x = ps[i]->async_read; x->next; x = x->next);
                    x->next = deps;
                    deps = ps[i]->async_read;
                    ps[i]->async_read = NULL; // No free
                }
            }

            // Wait async_write
            x = (async_list*)malloc(sizeof(async_list));
            x->n = ps[i]->async_write;
            x->timestamp = ps[i]->timestamp;
            x->next = deps;
            deps = x;

            /* Set async */
            if (async < 0) async = deps->n;

            /* Update async_write & async_read */
            if (arg->attr & JACC_WRITTEN) {
                // Update async_write
                ps[i]->async_write = async;
                ps[i]->timestamp = JACC_TIMESTAMP;
            }
            else {
                bool dup = false;

                for (async_list *x = ps[i]->async_read; x && !dup; x = x->next) {
                    if (x->n == async) {
                        dup = true;
                        x->timestamp = JACC_TIMESTAMP;
                    }
                }

                if (!dup) {
                    // Add to async_read
                    x = (async_list*)malloc(sizeof(async_list));
                    x->n = async;
                    x->timestamp = JACC_TIMESTAMP;
                    x->next = ps[i]->async_read;
                    ps[i]->async_read = x;
                }
            }
        }

        // Wait for implicit arrays or reduction (and w/ written present vars)
        wait = (!(arg->attr & JACC_PRESENT) && (arg->attr & JACC_ARRAY)) ||
            (is_dereferenced(arg)) || wait;
    }

    if (async < 0) {
        JACC_ASYNC_INC;
        async = JACC_ASYNC_NUM;
    }

    // No free
    for (async_list *x = deps; x; x = x->next) {
        bool dup = false;
        if (async == x->n) continue;
        for (async_list *y = deps; y != x; y = y->next) {
            if (x->n != y->n) continue;
            dup = true;
            break;
        }
        if (dup) continue;
        for (async_list *y = x->next; y; y = y->next) {
            if (x->n != y->n) continue;
            if (x->timestamp < y->timestamp) x->timestamp = y->timestamp;
        }
        if (JACC_ASYNC_SOLVED[x->n][async] < x->timestamp) {
            acc_wait_async(x->n, async);
            JACC_ASYNC_SOLVED[x->n][async] = JACC_TIMESTAMP;
        }
    }

    acc_set_default_async(async);

    if (wait) {
        acc_wait_async(async, acc_async_sync);
        async = acc_async_sync;
    }

    if (!OPTIMIZED)
        jacc_perform_kernel(kernel_code, kernel_arg, async);
    else
        jacc_perform_kernel_optimized(kernel_code, kernel_arg, async);
}

int
jacc_async_in(void *a, size_t len)
{
    JACC_ASYNC_INC;
    JACC_TIMESTAMP++;

    JaccPresent *p = malloc(sizeof(JaccPresent));
    p->addr        = a;
    p->len         = len;
    p->async_write = JACC_ASYNC_NUM;
    p->timestamp   = JACC_TIMESTAMP;
    p->async_read  = NULL;

    rb_tree_insert(JACC_PRESENT_TREE, p);

    return JACC_ASYNC_NUM;
}

int
jacc_async_update(void *a, size_t len, bool in)
{
    JaccPresent *p = rb_tree_find(JACC_PRESENT_TREE,
                                  & (JaccPresent) { .addr = a, .len = len });

    if (!p) {
        fprintf(stderr, "[JACC] Not present: %p\n", a);
        exit(-1);
    }

    if (in) {
        JACC_TIMESTAMP++;

        for (async_list *alist = p->async_read; alist; alist = alist->next) {
            if (JACC_ASYNC_SOLVED[alist->n][p->async_write] < alist->timestamp) {
                acc_wait_async(alist->n, p->async_write);
                JACC_ASYNC_SOLVED[alist->n][p->async_write] = JACC_TIMESTAMP;
            }
        }

        p->async_read = NULL; // No free
        p->timestamp  = JACC_TIMESTAMP;
    }
    else
        JACC_ASYNC_OUT[p->async_write] = true;

    return p->async_write;
}

int
jacc_async_out(void *a, size_t len)
{
    int async = jacc_async_update(a, len, false);

    rb_tree_remove(JACC_PRESENT_TREE,
                   & (JaccPresent) { .addr = a, .len = len });

    return async;
}

void
jacc_copyin(void *a, size_t len)
{
    acc_copyin_async(a, len, jacc_async_in(a, len));
}

void
jacc_create(void *a, size_t len)
{
    acc_create_async(a, len, jacc_async_in(a, len));
}

void
jacc_copyout(void *a, size_t len)
{
    acc_copyout_async(a, len, jacc_async_out(a, len));
}

void
jacc_delete(void *a, size_t len)
{
    acc_delete_async(a, len, jacc_async_out(a, len));
}

void
jacc_update_device(void *a, size_t len)
{
    acc_update_device_async(a, len, jacc_async_update(a, len, true));
}

void
jacc_update_self(void *a, size_t len)
{
    acc_update_self_async(a, len, jacc_async_update(a, len, false));
}

extern void cudaStreamDestroy(void *);
extern void cudaStreamCreateWithFlags(void **, int);

void
jacc_init()
{
    DEBUG("jacc_init");

    sprintf(JACC_TEMPDIR, "/tmp/jacc.XXXXXX");

    memset(JACC_TABLE, 0,
           JACC_TABLE_SIZE * sizeof(struct JaccTableEntry *));

    if (!mkdtemp(JACC_TEMPDIR)) {
        perror("[JACC] Error on mkdtemp");
        exit(-1);
    }

    JACC_PRESENT_TREE = rb_tree_create(rbcmp);

    JACC_ASYNC_NUM = 0;

    JACC_TIMESTAMP = 0;

    for (int i = 0; i < JACC_MAX_ASYNC; i++)
        for (int j = 0; j < JACC_MAX_ASYNC; j++)
            JACC_ASYNC_SOLVED[i][j] = JACC_TIMESTAMP;

    acc_set_default_async(0);

    for (int i = 0; i < JACC_MAX_ASYNC; i++) {
        void *stream;
        cudaStreamDestroy(acc_get_cuda_stream(i));
        cudaStreamCreateWithFlags(&stream, 1);
        acc_set_cuda_stream(i, stream);
    }

    OPTIMIZED = false;
}

void
jacc_wait()
{
    for (int i = 0; i < JACC_MAX_ASYNC; i++) {
        if (JACC_ASYNC_OUT[i]) {
            acc_wait(i);
            JACC_ASYNC_OUT[i] = false;
        }
    }
}

void
jacc_close()
{
    DEBUG("jacc_close");

    for (int i = 0; i < JACC_TABLE_SIZE; i++)
        jacc_table_entry_free(JACC_TABLE[i]);

    if (!rmdir(JACC_TEMPDIR)) {
        perror("[JACC] Error on rmdir");
        exit(-1);
    }

    if (JACC_PRESENT_TREE != NULL)
        rb_tree_dealloc(JACC_PRESENT_TREE, NULL);
}

void *__jacc_malloc(size_t len)
{
    void *ret = acc_malloc(len);

    jacc_async_in(ret, len);

    return ret;
}

void __jacc_free(void *a)
{
    acc_free(a);

    jacc_async_out(a, 1);
}

void jacc_acc_init(acc_device_t devicetype)
{}

void jacc_acc_shutdown(acc_device_t devicetype)
{}

struct JaccTableEntry *
jacc_jit_optimize(struct JaccTableEntry *entry)
{
    int kernel_count = entry->id;

    char code_path[0xff], lib_path[0xff], command[0xff], kernel_name[0xff];

    sprintf(code_path, "%s/%d_opt.c", JACC_TEMPDIR, kernel_count);
    sprintf(lib_path, "%s/%d_opt.so", JACC_TEMPDIR, kernel_count);
    sprintf(command,
            STR(JACC_OPENACC) " "
            STR(JACC_OPENACC_OPTION) " -shared -o %s %s",
            lib_path, code_path);
    sprintf(kernel_name, "jacc_jit_kernel%d_opt", kernel_count);

    FILE *fp;

    if ((fp = fopen(code_path, "w")) == NULL) {
        perror("[JACC] Error on fopen");
        exit(-1);
    }

    int nargs = 0;
    ffi_cif cif;
    ffi_type **types;
    void **values;

    int i = 0;
    for (JaccArg *arg = entry->arg; arg != NULL; arg = arg->next, i++) {
        if (!entry->fixed[i])
            nargs++;
        else {
            fprintf(fp, "#define %s ", arg->symbol);
            jacc_arg_print_data(fp, arg);
            fprintf(fp, "\n");
        }
    }

    // jacc_async
    nargs++;

    types  = (ffi_type **)calloc(nargs, sizeof(ffi_type *));
    values = (void **)calloc(nargs, sizeof(void *));

    // Args
    // - pointer used for array or var_write presented/reducted
    // - variable used for other variables
    fprintf(fp, "void %s (", kernel_name);

    i = 0;
    int j = 0;
    for (JaccArg *arg = entry->arg; arg != NULL; arg = arg->next, i++) {
        if (!entry->fixed[i]) {
            if (j != 0)
                fprintf(fp, ", ");

            bool dereference = is_dereferenced(arg);

            /* if (entry->restrictable[i]) */
            /*     jacc_arg_print_decl_restrict(fp, arg, dereference);  */
            /* else */
                jacc_arg_print_decl(fp, arg, dereference);

            if ((arg->attr & JACC_ARRAY) || dereference) {
                types[j] = &ffi_type_pointer;
            }
            else {
                types[j] = specifier_to_ffi_type(arg->type);
            }

            j++;
        }
    }

    // jacc_async
    fprintf(fp, ", int jacc_async");
    types[j] = specifier_to_ffi_type("int");

    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nargs,
                     &ffi_type_void, types) != FFI_OK) {
        perror("[JACC] Error on ffi_prep_cif");
        exit(-1);
    }

    fprintf(fp, ")\n{\n");

    // Scalar variables
    i = 0;
    for (JaccArg *arg = entry->arg; arg != NULL; arg = arg->next, i++) {
        if (is_dereferenced(arg)) {
            jacc_arg_print_decl(fp, arg, false);

            fprintf(fp, " = ");

            fprintf(fp, "(*(%s__ptr))", arg->symbol);

            fprintf(fp, ";\n");
        }
    }

    fputs("#define async async(jacc_async)\n", fp);
#ifdef __GNUC__
    fputs("#define num_workers(a)\n", fp);
#endif

    fputs((char*)entry->key, fp);

    // Assign to the address to which pointers indicate.
    // Only for var_write presented/reducted.
    for (JaccArg *arg = entry->arg; arg != NULL; arg = arg->next) {
        if (is_dereferenced(arg)) {
            fprintf(fp, "(*(%s__ptr)) = %s;\n", arg->symbol, arg->symbol);
        }
    }

    fprintf(fp, "\n}", kernel_name);

    fclose(fp);

    system(command);

    void *handle, *kernel;
    char *error;

    if (!(handle = dlopen(lib_path, RTLD_NOW))) {
        fprintf(stderr, "[JACC] Error on dlopen: %s\n", dlerror());
        exit(-1);
    }

    kernel = dlsym(handle, kernel_name);

    if ((error = dlerror()) != NULL) {
        fprintf(stderr, "[JACC] Error on dlsym: %s\n", error);
        exit(-1);
    }

    // No free
    /* free(entry->types); */
    /* free(entry->values); */

    entry->label = "O";
    entry->kernel = kernel;
    entry->nargs = nargs;
    entry->cif = cif;
    entry->types = types;
    entry->values =values;

    return entry;
}

void jacc_optimize()
{
    OPTIMIZED=true;

    for (int i = 0; i < JACC_TABLE_SIZE; i++) {
        struct JaccTableEntry *e = JACC_TABLE[i];

        while (e != NULL) {
            if (!strcmp(e->label, "N")) {
                fprintf(stderr, "[JACC] Optimizing: %d\n", e->id);
                jacc_jit_optimize(e);
            }
            e = e->next;
        }
    }
}
