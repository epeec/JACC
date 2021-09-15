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
#include <limits.h>
#include <omp.h>
#include <time.h>
#include <sys/time.h>

/* https://github.com/mirek/rb_tree */
#define RB_ITER_MAX_HEIGHT 64
struct rb_node;struct rb_tree;typedef int (*rb_tree_node_cmp_f)(struct rb_tree *self,struct rb_node *a,struct rb_node *b);typedef void (*rb_tree_node_f)(struct rb_tree *self,struct rb_node *node);struct rb_node{int red;struct rb_node *link[2];void *value;};struct rb_tree{struct rb_node *root;rb_tree_node_cmp_f cmp;size_t size;void *info;};struct rb_iter{struct rb_tree *tree;struct rb_node *node;struct rb_node *path[RB_ITER_MAX_HEIGHT];size_t top;void *info;};int rb_tree_node_cmp_ptr_cb(struct rb_tree *self,struct rb_node *a,struct rb_node *b);void rb_tree_node_dealloc_cb(struct rb_tree *self,struct rb_node *node);struct rb_node *rb_node_alloc();struct rb_node *rb_node_create(void *value);struct rb_node *rb_node_init(struct rb_node *self, void *value);void rb_node_dealloc(struct rb_node *self);struct rb_tree *rb_tree_alloc();struct rb_tree *rb_tree_create(rb_tree_node_cmp_f cmp);struct rb_tree *rb_tree_init(struct rb_tree *self,rb_tree_node_cmp_f cmp);void rb_tree_dealloc(struct rb_tree *self,rb_tree_node_f node_cb);void *rb_tree_find(struct rb_tree *self,void *value);int rb_tree_insert(struct rb_tree *self,void *value);int rb_tree_remove(struct rb_tree *self,void *value);size_t rb_tree_size(struct rb_tree *self);int rb_tree_insert_node(struct rb_tree *self,struct rb_node *node);int rb_tree_remove_with_cb(struct rb_tree *self,void *value,rb_tree_node_f node_cb);int rb_tree_test(struct rb_tree *self,struct rb_node *root);struct rb_iter *rb_iter_alloc();struct rb_iter *rb_iter_init(struct rb_iter *self);struct rb_iter *rb_iter_create();void rb_iter_dealloc(struct rb_iter *self);void *rb_iter_first(struct rb_iter *self,struct rb_tree *tree);void *rb_iter_last(struct rb_iter *self,struct rb_tree *tree);void *rb_iter_next(struct rb_iter *self);void *rb_iter_prev(struct rb_iter *self);struct rb_node *rb_node_alloc(){return malloc(sizeof(struct rb_node));}struct rb_node *rb_node_init(struct rb_node *self,void *value){if(self){self->red=1;self->link[0]=self->link[1]=NULL;self->value=value;}return self;}struct rb_node *rb_node_create(void *value){return rb_node_init(rb_node_alloc(),value);}void rb_node_dealloc(struct rb_node *self){if(self){free(self);}}static int rb_node_is_red(const struct rb_node *self){return self?self->red:0;}static struct rb_node *rb_node_rotate(struct rb_node *self,int dir){struct rb_node *result=NULL;if(self){result=self->link[!dir];self->link[!dir]=result->link[dir];result->link[dir]=self;self->red=1;result->red=0;}return result;}static struct rb_node *rb_node_rotate2(struct rb_node *self,int dir){struct rb_node *result=NULL;if(self){self->link[!dir]=rb_node_rotate(self->link[!dir],!dir);result=rb_node_rotate(self,dir);}return result;}int rb_tree_node_cmp_ptr_cb(struct rb_tree *self,struct rb_node *a,struct rb_node *b){return(a->value>b->value)-(a->value<b->value);}void rb_tree_node_dealloc_cb(struct rb_tree *self,struct rb_node *node){if(self){if(node){rb_node_dealloc(node);}}}struct rb_tree *rb_tree_alloc(){return malloc(sizeof(struct rb_tree));}struct rb_tree *rb_tree_init(struct rb_tree *self,rb_tree_node_cmp_f node_cmp_cb){if(self){self->root=NULL;self->size=0;self->cmp=node_cmp_cb?node_cmp_cb:rb_tree_node_cmp_ptr_cb;}return self;}struct rb_tree *rb_tree_create(rb_tree_node_cmp_f node_cb){return rb_tree_init(rb_tree_alloc(),node_cb);}void rb_tree_dealloc(struct rb_tree *self,rb_tree_node_f node_cb){if(self){if(node_cb){struct rb_node *node=self->root;struct rb_node *save=NULL;while(node){if(node->link[0]==NULL){save=node->link[1];node_cb(self,node);node=NULL;}else{save=node->link[0];node->link[0]=save->link[1];save->link[1]=node;}node=save;}}free(self);}}int rb_tree_test(struct rb_tree *self,struct rb_node *root){int lh,rh;if(root==NULL)return 1;else{struct rb_node *ln=root->link[0];struct rb_node *rn=root->link[1];if(rb_node_is_red(root)){if(rb_node_is_red(ln)||rb_node_is_red(rn)){printf("Red violation");return 0;}}lh=rb_tree_test(self,ln);rh=rb_tree_test(self,rn);if((ln!=NULL&&self->cmp(self,ln,root)>=0)||(rn!=NULL&&self->cmp(self,rn,root)<=0)){puts("Binary tree violation");return 0;}if(lh!=0&&rh!=0&&lh!=rh){puts("Black violation");return 0;}if(lh!=0&&rh!=0) return rb_node_is_red(root)?lh:lh+1;else return 0;}}void *rb_tree_find(struct rb_tree *self,void *value){void *result=NULL;if(self){struct rb_node node={.value=value};struct rb_node *it=self->root;int cmp=0;while(it){if((cmp=self->cmp(self,it,&node))){it=it->link[cmp<0];}else{break;}}result=it?it->value:NULL;}return result;}int rb_tree_insert(struct rb_tree *self,void *value){return rb_tree_insert_node(self,rb_node_create(value));}int rb_tree_insert_node(struct rb_tree *self,struct rb_node *node){if(self&&node){if(self->root==NULL){self->root=node;}else{struct rb_node head={0};struct rb_node *g,*t;struct rb_node *p,*q;int dir=0,last=0;t=&head;g=p=NULL;q=t->link[1]=self->root;while(1){if(q==NULL){p->link[dir]=q=node;}else if(rb_node_is_red(q->link[0])&&rb_node_is_red(q->link[1])){q->red=1;q->link[0]->red=0;q->link[1]->red=0;}if(rb_node_is_red(q)&&rb_node_is_red(p)){int dir2=t->link[1]==g;if(q==p->link[last]){t->link[dir2]=rb_node_rotate(g,!last);}else{t->link[dir2]=rb_node_rotate2(g,!last);}}if(self->cmp(self,q,node)==0){break;}last=dir;dir=self->cmp(self,q,node)<0;if(g!=NULL){t=g;}g=p,p=q;q=q->link[dir];}self->root=head.link[1];}self->root->red=0;++self->size;}return 1;}int rb_tree_remove_with_cb(struct rb_tree *self,void *value,rb_tree_node_f node_cb){if(self->root!=NULL){struct rb_node head={0};struct rb_node node={.value=value};struct rb_node *q,*p,*g;struct rb_node *f=NULL;int dir=1;q=&head;g=p=NULL;q->link[1]=self->root;while(q->link[dir]!=NULL){int last=dir;g=p,p=q;q=q->link[dir];dir=self->cmp(self,q,&node)<0;if(self->cmp(self,q,&node)==0){f=q;}if(!rb_node_is_red(q)&&!rb_node_is_red(q->link[dir])){if(rb_node_is_red(q->link[!dir])){p=p->link[last]=rb_node_rotate(q,dir);}else if(!rb_node_is_red(q->link[!dir])){struct rb_node *s=p->link[!last];if(s){if(!rb_node_is_red(s->link[!last])&&!rb_node_is_red(s->link[last])){p->red=0;s->red=1;q->red=1;}else{int dir2=g->link[1]==p;if(rb_node_is_red(s->link[last])){g->link[dir2]=rb_node_rotate2(p,last);}else if(rb_node_is_red(s->link[!last])){g->link[dir2]=rb_node_rotate(p,last);}q->red=g->link[dir2]->red=1;g->link[dir2]->link[0]->red=0;g->link[dir2]->link[1]->red=0;}}}}}if(f){void *tmp=f->value;f->value=q->value;q->value=tmp;p->link[p->link[1]==q]=q->link[q->link[0]==NULL];if(node_cb){node_cb(self,q);}q=NULL;}self->root=head.link[1];if(self->root!=NULL){self->root->red=0;}--self->size;}return 1;}int rb_tree_remove(struct rb_tree *self,void *value){int result=0;if(self){result=rb_tree_remove_with_cb(self,value,rb_tree_node_dealloc_cb);}return result;}size_t rb_tree_size(struct rb_tree *self){size_t result=0;if(self){result=self->size;}return result;}struct rb_iter *rb_iter_alloc(){return malloc(sizeof(struct rb_iter));}struct rb_iter *rb_iter_init(struct rb_iter *self){if(self){self->tree=NULL;self->node=NULL;self->top=0;}return self;}struct rb_iter *rb_iter_create(){return rb_iter_init(rb_iter_alloc());}void rb_iter_dealloc(struct rb_iter *self){if(self){free(self);}}static void *rb_iter_start(struct rb_iter *self,struct rb_tree *tree,int dir){void *result=NULL;if(self){self->tree=tree;self->node=tree->root;self->top=0;if(self->node!=NULL){while(self->node->link[dir]!=NULL){self->path[self->top++]=self->node;self->node=self->node->link[dir];}}result=self->node==NULL?NULL:self->node->value;}return result;}static void *rb_iter_move(struct rb_iter *self,int dir){if(self->node->link[dir]!=NULL){self->path[self->top++]=self->node;self->node=self->node->link[dir];while(self->node->link[!dir]!=NULL){self->path[self->top++]=self->node;self->node=self->node->link[!dir];}}else{struct rb_node *last=NULL;do{if(self->top==0){self->node=NULL;break;}last=self->node;self->node=self->path[--self->top];}while(last==self->node->link[dir]);}return self->node==NULL?NULL:self->node->value;}void *rb_iter_first(struct rb_iter *self,struct rb_tree *tree){return rb_iter_start(self,tree,0);}void *rb_iter_last(struct rb_iter *self,struct rb_tree *tree){return rb_iter_start(self,tree,1);}void *rb_iter_next(struct rb_iter *self){return rb_iter_move(self,1);}void *rb_iter_prev(struct rb_iter *self){return rb_iter_move(self,0);}

void cudaMemcpy(void *, const void *, size_t, int);
void cudaMemcpyAsync(void *, const void *, size_t, int, void *);
void cudaMemcpyPeerAsync(void *, int, void *, int, size_t, void *);
void cudaMemcpy2DAsync(void *, size_t, const void *, size_t, size_t, size_t, int, void *);
void cudaDeviceEnablePeerAccess(int, int);
void cudaStreamCreateWithFlags(void **, int);
void cudaStreamSynchronize(void *);
void cudaSetDevice(int);
void cuCtxSetCurrent(void *);
#define cudaMemcpyDefault 4

static int JACC_NUMGPUS = -1;
#define JACC_MAX_GPUS 10
#define JACC_MAX_SUBSTREAMS 8
#define JACC_DEVICE_TYPE acc_device_nvidia
#define CEIL(a, b) ((a + b - 1) / b)
static int JACC_ASYNC[JACC_MAX_GPUS];
static void *JACC_STREAM[JACC_MAX_GPUS];
static void *JACC_STREAM_SUB[JACC_MAX_GPUS][JACC_MAX_SUBSTREAMS];

static int DGX2 = -1;

#ifdef __PGIC__
#undef __GNUC__
#else
void cuCtxEnablePeerAccess(void *, unsigned int);
#endif

void
jacc_set_device_num(int device)
{
    acc_set_device_num(device, JACC_DEVICE_TYPE);
}
bool
jacc_set_device_num_with_cuda(int device)
{
    acc_set_device_num(device, JACC_DEVICE_TYPE);
#ifdef __GNUC__
    cudaSetDevice(device);
    cuCtxSetCurrent(acc_get_current_cuda_context());
#endif
    return true;
}

#define EACHGPU(n) \
    for (int n = 0; n < JACC_NUMGPUS && jacc_set_device_num_with_cuda(n); n++)

double jacc_time()
{
  struct timespec tp;

  if (clock_gettime(CLOCK_MONOTONIC_RAW, &tp) != 0)
    perror("clock_gettime failed");

  return (tp.tv_sec + tp.tv_nsec * 1.0e-9) * 1.0e3;
}


#if !defined(JACC_OPENACC) || !defined(JACC_OPENACC_OPTION)
#ifdef __GNUC__
#define JACC_OPENACC gfortran
#define JACC_OPENACC_OPTION -fopenacc -foffload=nvptx-none -O3 -fno-strict-aliasing -foffload=-lm -fPIC
#else
#define JACC_OPENACC pgf90
#define JACC_OPENACC_OPTION -acc -mp -ta=tesla:cc70 -O2 -Mcuda
#endif
#endif

#define XSTR(s) #s
#define STR(s) XSTR(s)
#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

#define JaccArg __JaccArg
#define jacc_kernel_push __jacc_kernel_push
#define jacc_malloc __jacc_malloc
#define jacc_free __jacc_free
#define jacc_copyin __jacc_copyin
#define jacc_create __jacc_create
#define jacc_copyout __jacc_copyout
#define jacc_delete __jacc_delete
#define jacc_malloc __jacc_malloc
#define jacc_free __jacc_free
#define jacc_update_self __jacc_update_self
#define jacc_update_device __jacc_update_device
#define jacc_init __jacc_init
#define jacc_close __jacc_close
#define jacc_wait __jacc_wait
#define jacc_acc_init __jacc_acc_init
#define jacc_acc_shutdown __jacc_acc_shutdown
#define jacc_optimize __jacc_optimize
#define jacc_arg_build __jacc_arg_build
#define JACC_ARRAY __JACC_ARRAY
#define JACC_STATIC __JACC_STATIC
#define JACC_PRESENT __JACC_PRESENT
#define JACC_REDUCTED __JACC_REDUCTED
#define JACC_DIST __JACC_DIST
#define JACC_WRITTEN __JACC_WRITTEN
#define JACC_READ __JACC_READ

static bool OPTIMIZED;
static double accmu;

static char JACC_TEMPDIR[0xff];

#define JACC_MAX_ARGS 500

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
    size_t *elmsizes;

    bool *restrictable;

    int warmed;
    int multi;
    int tuned;
    double exec_to_writesize;
    double exec_ave;
    long exec_count;
    long exec_over;

    struct JaccArg *arg;

    size_t id; // For the purpose of debugging
    struct JaccTableEntry *next;
};

struct JaccTableEntry *JACC_TABLE[JACC_TABLE_SIZE];

typedef struct JaccPresent {
    void *addr;
    size_t len;
    void **dev_addr;
    size_t count;
} JaccPresent;

typedef struct rb_tree rb_tree;
typedef struct rb_node rb_node;

int
rbcmp(rb_tree *self, rb_node *node_a, rb_node *node_b)
{
    JaccPresent *a = (JaccPresent*)node_a->value;
    JaccPresent *b = (JaccPresent*)node_b->value;

    if ((a->addr <= b->addr && (size_t)a->addr + a->len - 1 >= (size_t)b->addr) ||
        (b->addr <= a->addr && (size_t)b->addr + b->len - 1 >= (size_t)a->addr))
        return 0;

    return (a->addr > b->addr) - (a->addr < b->addr);
}

rb_tree *JACC_PRESENT_TREE = NULL;

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

bool is_dereferenced(JaccArg *arg)
{
    return (!(arg->attr & JACC_ARRAY) &&
            arg->attr & JACC_WRITTEN &&
            ((arg->attr & JACC_PRESENT) ||
             ((arg->attr & JACC_REDUCTED))));
}

void jacc_arg_build(const char *type, const char *symbol, void *addr,
                    size_t size, int attr, int dimnum, int splitdim,
                    size_t *lbound, size_t *ubound, __JaccArg **next)
{
    __JaccArg *r = malloc(sizeof(__JaccArg));

    int is_array = attr & __JACC_ARRAY;
    size_t split_dimsize = is_array ? (ubound[splitdim] - lbound[splitdim] + 1) : 0;
    size_t memdepth = 1;

    if (is_array)
        for (int i = splitdim - 1; i >= 0; i--)
            memdepth *= ubound[i] - lbound[i] + 1;

    size_t length = 1, elmsize = size;

    if (is_array) {
        for (int i = dimnum - 1; i >= 0; i--)
            length *= ubound[i] - lbound[i] + 1;

        elmsize = size / length;
    }

    int *_lb, *_ub;
    _lb = malloc(sizeof(int) * dimnum);
    _ub = malloc(sizeof(int) * dimnum);
    for (int i = 0; i < dimnum; i++)
        _lb[i] = (int)lbound[i], _ub[i] = (int)ubound[i];

    *r = ((struct __JaccArg){
            type, symbol, NULL, addr,
            size, attr, dimnum, _lb, _ub,
            splitdim, split_dimsize, memdepth, elmsize, *next
         });

    if (is_array)
        r->data = &(r->addr);
    else
        r->data = r->addr;

    *next = r;
}

JaccArg *
jacc_arg_copy(JaccArg *arg)
{
    if (arg == NULL) return NULL;

    JaccArg *ret = (JaccArg *)malloc(sizeof(JaccArg));
    JaccArg *next = jacc_arg_copy(arg->next);

    ret = memcpy(ret, arg, sizeof(JaccArg));
    // For entry->args (a2); used for write buffer for non-primary GPUs
    if (is_dereferenced(arg)) {
        ret->data = malloc(arg->size);
        ret->data = memcpy(ret->data, arg->data, arg->size);
    }
    ret->next = next;
    return ret;
}

void
jacc_arg_free(JaccArg *arg)
{
    if (arg == NULL) return;

    JaccArg *next = arg->next;

    if (is_dereferenced(arg))
        free(arg->data);
    free(arg);
    jacc_arg_free(next);
}

struct JaccTableEntry *
jacc_table_insert(const char *code, char *label, void *kernel, size_t nargs,
                  ffi_cif cif, ffi_type **types, void **values, size_t *elmsizes,
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
    entry->elmsizes = elmsizes;

    entry->restrictable = (bool*)malloc(sizeof(bool) * nargs);
    for (int i = 0; i < nargs; i++)
        entry->restrictable[i] = true;

    entry->warmed = 0;
    entry->multi = 0;
    entry->tuned = 0;
    entry->exec_to_writesize = 0.0;
    entry->exec_ave = 0.0;
    entry->exec_count = 0;
    entry->exec_over = 0;

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

struct JaccTableEntry *
jacc_jit(const char *kernel_code, JaccArg *kernel_arg)
{
    static int kernel_count = 0; kernel_count += 1;

    if (strncmp(JACC_TEMPDIR, "/tmp/jacc.", 10) != 0)
        jacc_init();

    char code_path[0xff], lib_path[0xff], command[0xff], kernel_name[0xff];

    sprintf(code_path, "%s/%d.f90", JACC_TEMPDIR, kernel_count);
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
    size_t *elmsizes;

    for (JaccArg *arg = kernel_arg; arg != NULL; arg = arg->next) {
        nargs += 1;
        nargs += (arg->attr & JACC_ARRAY) ? 2 : 0;
        nargs += ((arg->attr & JACC_ARRAY) && (arg->attr & JACC_WRITTEN)) ? 2 : 0;
        nargs += (arg->attr & JACC_REDUCTED) ? 1 : 0;
    }
    nargs++; // __gpuid
    nargs++; // __gpunum

    types  = (ffi_type **)calloc(nargs, sizeof(ffi_type *));
    values = (void **)calloc(nargs, sizeof(void *));
    elmsizes = (size_t *)calloc(nargs, sizeof(size_t));

    fprintf(fp, "MODULE tmp%d \n INTERFACE\n  SUBROUTINE GOMP_barrier() BIND(C)\n  END SUBROUTINE GOMP_barrier\n  END INTERFACE\n END MODULE\n", kernel_count);

    // Args
    fprintf(fp, "subroutine %s (", kernel_name);

    int i = 0;
    for (JaccArg *arg = kernel_arg; arg != NULL; arg = arg->next, i++) {
        if (i != 0)
            fprintf(fp, ",&\n");

        fprintf(fp, "%s", arg->symbol);

        if (!(arg->attr & JACC_ARRAY) && !is_dereferenced(arg))
            fprintf(fp, "__a");

        types[i] = &ffi_type_pointer;

        if (arg->attr & JACC_ARRAY) {
            elmsizes[i] = arg->elmsize;

            fprintf(fp, ", %s__rlb, %s__rub", arg->symbol, arg->symbol);

            i++;
            types[i] = &ffi_type_pointer;
            i++;
            types[i] = &ffi_type_pointer;
        }

        if ((arg->attr & JACC_ARRAY) && (arg->attr & JACC_WRITTEN)) {
            i++;
            types[i] = &ffi_type_pointer;
            i++;
            types[i] = &ffi_type_pointer;

            fprintf(fp, ", %s__lb, %s__ub", arg->symbol, arg->symbol);
        }

        if (arg->attr & JACC_REDUCTED) {
            i++;
            types[i] = &ffi_type_pointer;
            fprintf(fp, ", %s__shared", arg->symbol);
        }
    }

    if (i != 0) fprintf(fp, ", j__gpuid");
    if (i != 0) fprintf(fp, ", j__gpunum");
    types[i] = &ffi_type_pointer;
    types[i+1] = &ffi_type_pointer;

    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nargs,
                     &ffi_type_void, types) != FFI_OK) {
        perror("[JACC] Error on ffi_prep_cif");
        exit(-1);
    }

    fprintf(fp, ") BIND(C)\n");

    fprintf(fp, "use tmp%d\n", kernel_count);

    // TODO: parameter

    // Args
    for (JaccArg *arg = kernel_arg; arg != NULL; arg = arg->next) {
        int is_complex = (strstr(arg->type, "complex") != NULL);

        if (arg->attr & JACC_ARRAY) {
            fprintf(fp, "integer :: %s__rlb(%d), %s__rub(%d)\n",
                    arg->symbol, arg->dimnum, arg->symbol, arg->dimnum);

            fprintf(fp, "%s(%zu), parameter :: %s__zero = ",
                    arg->type, arg->elmsize / (is_complex ? 2 : 1), arg->symbol);
            if (is_complex)
                fprintf(fp, "(0._%zu, 0._%zu)", arg->elmsize / 2, arg->elmsize / 2);
            else
                fprintf(fp, "%s_%zu",
                        strstr(arg->type, "integer") ? "0" : "0.", arg->elmsize);
            fprintf(fp, "\n");
        }

        if (arg->attr & JACC_WRITTEN)
            fprintf(fp, "%s(%zu), target :: %s",
                    arg->type, arg->elmsize / (is_complex ? 2 : 1), arg->symbol);
        else
            fprintf(fp, "%s(%zu) :: %s",
                    arg->type, arg->elmsize / (is_complex ? 2 : 1), arg->symbol);

        if (arg->attr & JACC_ARRAY) {
            fprintf(fp, "(");
            for (int d = 1; d <= arg->dimnum; d++) {
                if (d != 1) fprintf(fp, ", ");
                fprintf(fp, "%s__rlb(%d):%s__rub(%d)",
                        arg->symbol, d, arg->symbol, d);
            }
            fprintf(fp, ")");
        }
        fprintf(fp, "\n");

        if (arg->attr & JACC_REDUCTED)
            fprintf(fp, "%s(%zu) :: %s__shared(0:j__gpunum)\n",
                    arg->type, arg->elmsize / (is_complex ? 2 : 1), arg->symbol);

        if (!(arg->attr & JACC_ARRAY) && !is_dereferenced(arg))
            fprintf(fp, "%s(%zu) :: %s__a\n",
                    arg->type, arg->elmsize / (is_complex ? 2 : 1), arg->symbol);

        if ((arg->attr & JACC_ARRAY) && (arg->attr & JACC_WRITTEN))
            fprintf(fp, "integer :: %s__lb, %s__ub\n",
                    arg->symbol, arg->symbol);
    }

    fprintf(fp, "integer :: j__gpuid, j__gpunum, i\n");

    // %s = %s__a  (Use local variables for read-only use to avoid PGI's bugs)
    for (JaccArg *arg = kernel_arg; arg != NULL; arg = arg->next)
        if (!(arg->attr & JACC_ARRAY) && !is_dereferenced(arg))
            fprintf(fp, "%s = %s__a\n", arg->symbol, arg->symbol);

/*     fputs("#define DEVICEPTR PRESENT\n", fp); */
/* #ifdef __GNUC__ */
/*     fputs("#define NUM_WORKERS(a)\n", fp); */
/* #endif */

    fputs(kernel_code, fp);

    fputs("\n", fp);

    for (JaccArg *arg = kernel_arg; arg != NULL; arg = arg->next)
        if (arg->attr & JACC_REDUCTED) {
            fprintf(fp, "%s__shared(j__gpuid) = %s\n", arg->symbol, arg->symbol);
        }

#ifdef __GNUC__
    fputs("call GOPM_barrier\n", fp);
#else
    fputs("!$omp barrier\n", fp);
#endif

    for (JaccArg *arg = kernel_arg; arg != NULL; arg = arg->next)
        if (arg->attr & JACC_REDUCTED) {
            fprintf(fp, "do i=1,j__gpunum-1\n");
            fprintf(fp, "  %s = ", arg->symbol);
            if (arg->splitdim >= 0 && arg->splitdim <= 4)
                fprintf(fp, "%s %s %s__shared(i)\n",
                        arg->symbol,
                        (arg->splitdim == 0) ? "+" :
                        (arg->splitdim == 1) ? "-" :
                        (arg->splitdim == 2) ? "*" :
                        (arg->splitdim == 3) ? ".and." :
                        (arg->splitdim == 4) ? ".or." : "NONE",
                        arg->symbol);
            else
                fprintf(fp, "%s(%s, %s__shared(i))\n",
                        (arg->splitdim == 5) ? "iand" :
                        (arg->splitdim == 6) ? "ior" :
                        (arg->splitdim == 7) ? "ieor" :
                        (arg->splitdim == 8) ? "min" :
                        (arg->splitdim == 9) ? "max" : "NONE",
                        arg->symbol, arg->symbol);

            fprintf(fp, "end do\n");
        }

    fprintf(fp, "\nend subroutine %s", kernel_name);

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
                             nargs, cif, types, values, elmsizes,
                             kernel_arg, kernel_count);
}

JaccPresent *
jacc_lookup_present_if(void *a)
{
    return rb_tree_find(JACC_PRESENT_TREE,
                        & (JaccPresent) { .addr = a, .len = 1 });
}

JaccPresent *
jacc_lookup_present(void *a)
{
    JaccPresent *p = jacc_lookup_present_if(a);

    if (!p) {
        fprintf(stderr, "[JACC] Not present: %p\n", a);
        exit(-1);
    }

    return p;
}

void
jacc_perform_kernel(const char *kernel_code, JaccArg *kernel_arg)
{
    struct JaccTableEntry *entry = jacc_table_find(kernel_code, "N");

    entry = (entry != NULL) ? entry : jacc_jit(kernel_code, kernel_arg);

    JaccPresent *ps[JACC_MAX_ARGS];
    bool is_read[JACC_MAX_ARGS];
    bool is_written[JACC_MAX_ARGS];
    bool is_conflicted[JACC_MAX_ARGS];

    int i = 0;
    for (JaccArg *a1 = kernel_arg, *a2 = entry->arg;
         a1 != NULL; a1 = a1->next, a2 = a2->next, i++) {
        if (a1->attr & JACC_ARRAY) {
            ps[i] = jacc_lookup_present_if(a1->addr);
            is_read[i]    = a1->attr & JACC_READ;
            is_written[i] = a1->attr & JACC_WRITTEN;
            is_conflicted[i] = false;
        }
        else
            ps[i] = NULL;

        for (int j = 0; j < i; j++) {
            if (ps[i] && ps[j] && ps[i]->addr == ps[j]->addr) {
                entry->restrictable[i] = false;
                entry->restrictable[j] = false;

                if ((is_written[i] && is_read[j]) ||
                    is_conflicted[i] || is_conflicted[j]) {
                    is_conflicted[i] = true;
                    is_conflicted[j] = true;
                }
            }
        }

        bool dereferenced = is_dereferenced(a1);

        if (dereferenced && JACC_NUMGPUS >= 2)
            memcpy(a2->data, a1->data, a1->size);

        // All aguments are passed as pointers
        entry->values[i] = &(a1->addr);

        if ((a1->attr & JACC_ARRAY)) {
            i++;
            ps[i] = NULL;
            i++;
            ps[i] = NULL;
        }

        if ((a1->attr & JACC_ARRAY) && (a1->attr & JACC_WRITTEN)) {
            i++;
            ps[i] = NULL;
            i++;
            ps[i] = NULL;
        }

        if (a1->attr & JACC_REDUCTED) {
            i++;
            ps[i] = NULL;
        }
    }

    // __gpuid
    ps[i] = NULL;
    ps[i+1] = NULL;

    long lb_set[JACC_MAX_GPUS][JACC_MAX_ARGS];
    long ub_set[JACC_MAX_GPUS][JACC_MAX_ARGS];
    bool copied[JACC_MAX_ARGS];
    long pad[JACC_MAX_ARGS];
    long split_dimsize[JACC_MAX_ARGS];
    long memdepth[JACC_MAX_ARGS];
    long memheight[JACC_MAX_ARGS];
    long mempitch[JACC_MAX_ARGS];
    long tail[JACC_MAX_ARGS];

    i = 0;
    for (JaccArg *arg = kernel_arg, *a2 = entry->arg;
         arg != NULL; arg = arg->next, a2 = a2->next, i++) {
        copied[i] = false;

        if ((arg->attr & JACC_ARRAY) && (arg->attr & JACC_WRITTEN)) {
            pad[i] = ps[i] ? (long)arg->addr - (long)ps[i]->addr : 0;
            split_dimsize[i] = arg->split_dimsize;
            memdepth[i] = arg->memdepth;

            long elmnum = ps[i] ? (ps[i]->len - pad[i]) / entry->elmsizes[i] : 0;

            if (split_dimsize[i] == 0)
                split_dimsize[i] = elmnum / memdepth[i];

            mempitch[i] = split_dimsize[i] * memdepth[i];
            memheight[i] = elmnum / mempitch[i];

            tail[i] = split_dimsize[i] - 1;
            long block = CEIL(tail[i] + 1, JACC_NUMGPUS);

            for (int n = 0; n < JACC_NUMGPUS; n++) {
                lb_set[n][i] = (n == 0) ? 0 : (ub_set[n-1][i] + 1);
                ub_set[n][i] = (n != JACC_NUMGPUS - 1) ?
                    (lb_set[n][i] + block - 1) : tail[i];

                if (ub_set[n][i] > tail[i])
                    ub_set[n][i] = tail[i];

                if (lb_set[n][i] > ub_set[n][i])
                    ub_set[n][i] = lb_set[n][i] - 1;

                copied[i] = true;
                copied[i+1] = false;
                copied[i+2] = false;

                if (is_conflicted[i] || !(arg->attr & JACC_DIST)) {
                    // Write all
                    copied[i] = false;
                    lb_set[n][i] = 0;
                    ub_set[n][i] = tail[i];
                }
            }

            // Merge two writes which are not conflicted
            for (int j = 0; j < i; j++) {
                if (ps[i] && ps[j]
                    && ps[i]->addr == ps[j]->addr && copied[j]) {

                    int lastgpu = JACC_NUMGPUS - 1;

                    int larger  =
                        (ub_set[lastgpu][j] >= ub_set[lastgpu][i]) ? j : i;
                    int smaller = (larger == j) ? i : j;

                    copied[smaller] = false;
                    long sub =
                        ub_set[lastgpu][larger] - ub_set[lastgpu][smaller];

                    for (int n = 0; n < JACC_NUMGPUS; n++) {
                        if (memdepth[larger] != memdepth[smaller]) {
                            // Different dimension split => duplicate computation on smaller one
                            lb_set[n][smaller] = 0;
                            ub_set[n][smaller] = tail[i];
                        }
                        else {
                            lb_set[n][smaller] = lb_set[n][larger] - sub;
                            ub_set[n][smaller] = ub_set[n][larger] - sub;
                        }
                    }
                }
            }

            i += 2;
        }

        if ((arg->attr & JACC_ARRAY)) {
            copied[i+1] = false;
            copied[i+2] = false;
            i += 2;
        }

        if (arg->attr & JACC_REDUCTED) {
            copied[i+1] = NULL;
            i++;
        }
    }

    // __gpuid
    copied[i] = false;
    copied[i+1] = false;

    double reduction_pool[JACC_MAX_ARGS][JACC_MAX_GPUS];

#pragma omp parallel num_threads(JACC_NUMGPUS) private(i)
    {
        int n = omp_get_thread_num();

        jacc_set_device_num_with_cuda(n);

        void *avalue[JACC_MAX_ARGS];
        void *avalue_p[JACC_MAX_ARGS];
        long boundary_padded[JACC_MAX_ARGS];
        double writesize = 0.0;

        i = 0;
        for (JaccArg *arg = kernel_arg, *a2 = entry->arg;
             arg != NULL; arg = arg->next, a2 = a2->next, i++) {
            avalue[i] = entry->values[i];

            if (n >= 1 && is_dereferenced(arg))
                avalue[i] = &(a2->data);

            if (copied[i]) {
                long memwidth = (ub_set[n][i] - lb_set[n][i] + 1) * memdepth[i] * entry->elmsizes[i];
                writesize += memwidth * memheight[i];
            }

            if ((arg->attr & JACC_ARRAY) && (arg->attr & JACC_WRITTEN)) {
                if (!entry->multi) {
                    copied[i] = false;
                    lb_set[n][i] = 0;
                    ub_set[n][i] = tail[i];
                }
                if (!ps[i]) {
                    copied[i] = false;
                    lb_set[n][i] = 0;
                    ub_set[n][i] = split_dimsize[i] - 1;
                }

                avalue[i+1] = &(arg->lbound);
                avalue[i+2] = &(arg->ubound);
                boundary_padded[i+3] = lb_set[n][i] + arg->lbound[arg->splitdim];
                boundary_padded[i+4] = ub_set[n][i] + arg->lbound[arg->splitdim];
                avalue_p[i+3] = &(boundary_padded[i+3]);
                avalue_p[i+4] = &(boundary_padded[i+4]);
                avalue[i+3] = &avalue_p[i+3];
                avalue[i+4] = &avalue_p[i+4];

                i += 4;
            }
            else if ((arg->attr & JACC_ARRAY)) {
                avalue[i+1] = &(arg->lbound);
                avalue[i+2] = &(arg->ubound);

                i += 2;
            }

            if (arg->attr & JACC_REDUCTED) {
                avalue_p[i+1] = &(reduction_pool[i][0]);
                avalue[i+1] = &avalue_p[i+1];
                i++;
            }
        }

        // __gpuid
        void *n_p = &n, *ng_p = &JACC_NUMGPUS;
        avalue[i] = &n_p;
        avalue[i+1] = &ng_p;

        double start = jacc_time();

        // Kernel execution
        int kernel_result;
        ffi_call(&entry->cif, (void (*)(void))entry->kernel,
                 &kernel_result, avalue);

/* #pragma omp barrier (now in function) */

        double kernel = jacc_time();

        if (OPTIMIZED && n == 0)
            accmu += kernel - start;

        // P2P communication
        int count = 0;
        for (i = 0; i < entry->nargs; i++)
            if (copied[i] && lb_set[n][i] <= ub_set[n][i] && lb_set[n][i] >= 0) {
                long head = pad[i] + lb_set[n][i] * memdepth[i] * entry->elmsizes[i];
                long memwidth = (ub_set[n][i] - lb_set[n][i] + 1) * memdepth[i] * entry->elmsizes[i];

                for (int n2 = 0; n2 < JACC_NUMGPUS; n2++)
                    if (n2 != n) {
                        cudaMemcpy2DAsync(
                            (void*)((size_t)ps[i]->dev_addr[n2] + head), mempitch[i] * entry->elmsizes[i],
                            (void*)((size_t)ps[i]->dev_addr[n]  + head), mempitch[i] * entry->elmsizes[i],
                            memwidth, memheight[i], cudaMemcpyDefault,
                            JACC_STREAM_SUB[n][count % JACC_MAX_SUBSTREAMS]);
                        count++;
                    }
            }

        for (i = 0; i < JACC_MAX_SUBSTREAMS && i < count; i++)
            cudaStreamSynchronize(JACC_STREAM_SUB[n][i]);

        double comm = jacc_time();

        if (!entry->multi && !entry->tuned && JACC_NUMGPUS > 1 &&
            n == 0 && writesize > 0.0 && entry->warmed >= 3){
            // Use multi-gpu if Exec > Exec / NUMGPU + WriteSize / Peak_Throughput
            //   => P2P_Latency * (NUMGPU / (NUMGPU-1)) < exec / write-size
            double e_to_ws = (kernel - start) / writesize;

            entry->exec_to_writesize =
                (entry->exec_to_writesize * entry->exec_count + e_to_ws)
                / (entry->exec_count + 1);
            entry->exec_count++;

            // Peak_Throughput = 25GB/s -> 25 / 1000 GB/ms
            // DGX-2: 150/NUMGPUS GB/s
            double p2p_latency = 1 / ((DGX2 ? 150.0 / JACC_NUMGPUS : 25.0) * 1000.0 * 1000.0);

            if ((p2p_latency * JACC_NUMGPUS) / (JACC_NUMGPUS - 1) < e_to_ws) {
                entry->exec_over++;
            }

            if (entry->exec_over >= 5) {
                entry->multi = 5;
                entry->tuned = 1;
                entry->exec_count = 0;
            }

                /* for (i = 0; i < entry->nargs; i++) { */
                /*     if (copied[i]) { */
                /*         printf("%d %d: kernel: %lf comm-size: %lf (%ld ~ %ld)\n", entry->id, n, (kernel - start), writesize, lb_set[n][i], ub_set[n][i]);  */
                /*     } */
                /*     } */

        }

        else if (entry->multi && n == 0) {
            double benefit =
                MIN(entry->exec_to_writesize * writesize,
                    (kernel - start) * JACC_NUMGPUS) - (comm - start);

            /* printf("%d: %lf kernel: %lf comm: %lf\n", entry->id, benefit, kernel - start, comm - kernel); */

            entry->exec_ave =
                (entry->exec_ave * entry->exec_count + benefit) / (entry->exec_count + 1);

            entry->exec_count++;

            if (benefit < 0)
                entry->multi--;

            if (!entry->multi && entry->exec_ave > 0)
                entry->multi++;
        }

        entry->warmed++;
    }
}

void
jacc_perform_kernel_optimized(const char *kernel_code, JaccArg *kernel_arg)
{
}

void
jacc_kernel_push(const char *kernel_code, JaccArg **kernel_arg)
{
    int i = 0;
    for (JaccArg *arg = *kernel_arg; arg != NULL; arg = arg->next, i++) {
        if (((arg->attr & JACC_ARRAY) && (arg->attr & JACC_PRESENT)) &&
            !jacc_lookup_present_if(arg->addr)) {
            fprintf(stderr, "[JACC] Not present: %s (%p)\n", arg->symbol, arg->addr);
            exit(-1);
        }
    }

    jacc_perform_kernel(kernel_code, *kernel_arg);

    *kernel_arg = NULL;
}

void
jacc_data_in(void *a, size_t len, void **dev_a)
{
    JaccPresent *p;

    if (p = jacc_lookup_present_if(a)) {
        p->count++;
        return;
    }
 
    p = malloc(sizeof(JaccPresent));
    p->addr        = a;
    p->len         = len;
    p->dev_addr    = dev_a;
    p->count       = 1;

    rb_tree_insert(JACC_PRESENT_TREE, p);
}

void
jacc_data_out(void *a, size_t len)
{
    JaccPresent *p = jacc_lookup_present(a);

    p->count--;
}

void **jacc_mm(void *src, size_t len, bool copyin)
{
    if (strncmp(JACC_TEMPDIR, "/tmp/jacc.", 10) != 0)
        jacc_init();

    JaccPresent *p = src ? jacc_lookup_present_if(src) : NULL;
    if (p)
        return p->dev_addr;

    void **ret = malloc(sizeof(void *) * JACC_NUMGPUS);

    EACHGPU(n) {
        ret[n] = acc_malloc(len);
    }

    if (copyin) {
        EACHGPU(n) {
            cudaMemcpyAsync(ret[n], src, len,
                            cudaMemcpyDefault, JACC_STREAM[n]);
        }
        EACHGPU(n) {
            acc_wait(JACC_ASYNC[n]);
        }
    }

    if (src) {
        EACHGPU(n) {
            acc_map_data(src, ret[n], len);
        }
    }

    return ret;
}

void jacc_mm_remove(void *a, bool copyout)
{
    JaccPresent *p = jacc_lookup_present_if(a);

    if (p->count != 0)
        return;

    rb_tree_remove(JACC_PRESENT_TREE,
                   & (JaccPresent) { .addr = a, .len = 1 });

    if (copyout)
        cudaMemcpy(p->addr, p->dev_addr[0], p->len, cudaMemcpyDefault);

    EACHGPU(n) {
        acc_unmap_data(p->addr);
        acc_free(p->dev_addr[n]);
    }
}

void *jacc_malloc(size_t len)
{
    void **dev_a = jacc_mm(NULL, len, false);

    // Mapping deviceptr -> present
    EACHGPU(n) {
        acc_map_data(dev_a[0], dev_a[n], len);
    }

    jacc_data_in(dev_a[0], len, dev_a);

    return dev_a[0];
}

void jacc_free(void *a)
{
    jacc_data_out(a, 1);

    jacc_mm_remove(a, false);
}

void
jacc_copyin(void *a, size_t len)
{
    void **dev_a = jacc_mm(a, len, true);

    jacc_data_in(a, len, dev_a);
}

void
jacc_create(void *a, size_t len)
{
    void **dev_a = jacc_mm(a, len, false);

    jacc_data_in(a, len, dev_a);
}

void
jacc_copyout(void *a, size_t len)
{
    jacc_data_out(a, len);

    jacc_mm_remove(a, true);
}

void
jacc_delete(void *a, size_t len)
{
    jacc_data_out(a, len);

    jacc_mm_remove(a, false);
}

void
jacc_update_device(void *a, size_t len)
{
    JaccPresent *p = jacc_lookup_present(a);

    EACHGPU(n) {
        cudaMemcpyAsync((void*)((size_t)p->dev_addr[n] +
                                ((size_t)a - (size_t)p->addr)), a,
                        len, cudaMemcpyDefault, JACC_STREAM[n]);
    }
    EACHGPU(n) {
        acc_wait(JACC_ASYNC[n]);
    }
}

void
jacc_update_self(void *a, size_t len)
{
    JaccPresent *p = jacc_lookup_present(a);

    cudaMemcpy(a, (void*)((size_t)p->dev_addr[0] +
                          ((size_t)a - (size_t)p->addr)),
               len, cudaMemcpyDefault);
}

void
jacc_init()
{
    char *env_jacc_numgpus = getenv("JACC_NUMGPUS");
    accmu=0.0;

    if (env_jacc_numgpus != NULL)
        JACC_NUMGPUS = atoi(env_jacc_numgpus);
    else
        JACC_NUMGPUS = acc_get_num_devices(JACC_DEVICE_TYPE);

    DGX2 = (acc_get_num_devices(JACC_DEVICE_TYPE) == 16);

    for (int n = 0; n < JACC_NUMGPUS; n++) {
        jacc_set_device_num(n);
#ifdef __GNUC__
        void *stream;
        cudaStreamCreateWithFlags(&stream, 1);
        acc_set_cuda_stream(0, stream);
        JACC_ASYNC[n] = 0;
        JACC_STREAM[n] = stream;
#else
        JACC_ASYNC[n] = acc_get_default_async();
        JACC_STREAM[n] = acc_get_cuda_stream(JACC_ASYNC[n]);
#endif

        for (int i = 0; i < JACC_MAX_SUBSTREAMS; i++)
            cudaStreamCreateWithFlags(&JACC_STREAM_SUB[n][i], 1);

        for (int n2 = 0; n2 < JACC_NUMGPUS; n2++)
            if (n != n2)
                cudaDeviceEnablePeerAccess(n2, 0);
    }

#ifdef __GNUC__
    for (int n = 0; n < JACC_NUMGPUS; n++) {
        jacc_set_device_num_with_cuda(n);
        void *ctx = acc_get_current_cuda_context();

        for (int n2 = 0; n2 < JACC_NUMGPUS; n2++) {
            if (n != n2) {
                jacc_set_device_num_with_cuda(n2);
                cuCtxEnablePeerAccess(ctx, 0);
            }
        }
    }
#endif

    sprintf(JACC_TEMPDIR, "/tmp/jacc.XXXXXX");

    memset(JACC_TABLE, 0,
           JACC_TABLE_SIZE * sizeof(struct JaccTableEntry *));

    if (!mkdtemp(JACC_TEMPDIR)) {
        perror("[JACC] Error on mkdtemp");
        exit(-1);
    }

    JACC_PRESENT_TREE = rb_tree_create(rbcmp);

    OPTIMIZED = false;
}

void
jacc_wait()
{
}

void
jacc_close()
{
    printf("kernel: %lf\n", accmu);
    
    for (int i = 0; i < JACC_TABLE_SIZE; i++)
        jacc_table_entry_free(JACC_TABLE[i]);

    if (!rmdir(JACC_TEMPDIR)) {
        perror("[JACC] Error on rmdir");
        exit(-1);
    }

    if (JACC_PRESENT_TREE != NULL)
        rb_tree_dealloc(JACC_PRESENT_TREE, NULL);
}

void jacc_acc_init(acc_device_t devicetype)
{}

void jacc_acc_shutdown(acc_device_t devicetype)
{}

struct JaccTableEntry *
jacc_jit_optimize(struct JaccTableEntry *entry)
{
    return NULL;
}

void jacc_optimize()
{
    OPTIMIZED=true;
}
