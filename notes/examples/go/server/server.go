package main

// a bunch of import statements -- only the ones needed are imported!
import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"
	"time"

	"github.com/google/uuid"
)

// some constants here, just for string naming conventions

const (
	statusQueued   = "queued"
	statusStarted  = "started"
	statusFinished = "finished"
	statusError    = "error"
)

const (
	jobGet = "get"
	jobPut = "put"
	jobEnd = "end"
)

// some types here
//  edit == a get/put/end job descriptor
//  DataStore == the underlying structure containing the hashmap
//    and other useful variables and functions

type edit struct {
	jobTime   time.Time // time at which this edit was created
	JobID     string    `json:"job_id"`
	JobType   string    `json:"job_type"`
	JobStatus string    `json:"job_status"`
	Name      string    `json:"name"`
	Value     string    `json:"value"`
}

// DataStore ...
type DataStore struct {
	data   map[string]int       // "whatever" -> ###
	timing map[string]time.Time // "key" -> last updated
	edits  map[string]*edit     // "jobid uuid4" -> message
	jobs   chan string
}

func performCalculation() {
	// you can imagine maybe we do some image processing here, etc
	time.Sleep(5 * time.Second)
}

// the (ds DataStore) here says that the function httpget is an instance
// function inside any DataStore type struct.
func (ds DataStore) httpget(w http.ResponseWriter, req *http.Request) {
	log.Printf("get %v", req)
	name := req.URL.Query().Get("name")
	if val, ok := ds.data[name]; ok {
		fmt.Fprintf(w, "%s: %d\n", name, val)
	} else {
		fmt.Fprintf(w, "%s not found\n", name)
	}
}

func (ds DataStore) httpput(w http.ResponseWriter, req *http.Request) {
	log.Printf("put %v", req)
	id := uuid.New().String() // create uuid for request
	ds.edits[id] = &edit{
		jobTime:   time.Now(),
		JobID:     id,
		JobType:   jobPut,
		JobStatus: statusQueued,
		Name:      req.URL.Query().Get("name"),
		Value:     req.URL.Query().Get("val"),
	}
	ds.jobs <- id // put request into processing channel
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]string{"job_id": id})
}

// retrieve job info from given input argument
func (ds DataStore) httpjob(w http.ResponseWriter, req *http.Request) {
	log.Printf("job %v", req)
	jobid := req.URL.Query().Get("id")
	if val, ok := ds.edits[jobid]; ok {
		// job exists
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(val)
	} else {
		fmt.Fprintf(w, "%s not found\n", jobid)
	}
}

func (ds DataStore) get(jobid string) {
	e := ds.edits[jobid]
	e.Value = strconv.Itoa(ds.data[e.Name])
	e.JobStatus = statusFinished
}

func (ds DataStore) put(jobid string) {
	e := ds.edits[jobid]
	if intval, err := strconv.Atoi(e.Value); err != nil || ds.timing[e.Name].After(e.jobTime) {
		e.JobStatus = statusError
	} else {
		ds.data[e.Name] = intval
		ds.timing[e.Name] = time.Now()
		e.JobStatus = statusFinished
	}
}

func main() {
	log.Println("starting simple server.")
	// option to run port on a given input argument
	port := 80
	if len(os.Args) > 1 {
		port, _ = strconv.Atoi(os.Args[1])
	}
	log.Printf(" running on port %v\n", port)
	// init http server
	server := &http.Server{Addr: ":" + strconv.Itoa(port)}
	// create a datastore
	store := DataStore{
		data:   map[string]int{},
		timing: map[string]time.Time{},
		edits:  map[string]*edit{},
		jobs:   make(chan string),
	}
	// set up endpoint stop
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel() // run cancel() right before main() returns
	// set up http path handlers
	http.HandleFunc("/get", store.httpget)
	http.HandleFunc("/put", store.httpput)
	http.HandleFunc("/job", store.httpjob)
	http.HandleFunc("/end", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(store.data) // send datastore to user
		cancel()
	})

	// spawn http server thread
	go func() {
		// always returns error. ErrServerClosed on graceful close
		log.Println("listening...")
		if err := server.ListenAndServe(); err != http.ErrServerClosed {
			// unexpected error. port in use?
			log.Fatalf("ListenAndServe(): %v", err)
		}
	}()

	// spawn data processing thread
	go func() {
		// process jobs from datastore channel
		for id := range store.jobs { // since this is a channel, it will iterate forever
			if e, ok := store.edits[id]; !ok {
				log.Fatalf("Could not find job_id %s\n", id)
			} else {
				e.JobStatus = statusStarted
				switch e.JobType {
				case jobEnd:
					cancel()
				case jobGet:
					go func() {
						performCalculation()
						store.get(id)
					}()
				case jobPut:
					go func() {
						performCalculation()
						store.put(id)
					}()
				default:
					log.Fatalf("unidentified jobType for edit %v\n", e)
				}
			}
		}
	}()

	// block wait for shutdown
	select {
	case <-ctx.Done():
		// graceful shutdown
		if err := server.Shutdown(ctx); err != nil {
			// failure/timeout shutting down the server gracefully
			panic(err)
		}
	} // no default case needed

	log.Printf("server closed.")
}
