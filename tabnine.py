import json
import os
import queue
import subprocess
import sys
import threading
import traceback
from threading import Thread
from typing import Dict, Optional, Union

try:
    import orjson as json_parser
except:
    import json as json_parser

import sexpdata
from epc.client import EPCClient
from epc.server import ThreadingEPCServer

from utils import get_tabnine_path, install_tabnine_at

TABNINE_PROTOCOL_VERSION = "1.0.14"
TABNINE_BINARIES_FOLDER = os.path.expanduser("~/.TabNine/")
DEFAULT_BUFFER_SIZE = 100000000


class TabNineSender(Thread):
    def __init__(self, process: subprocess.Popen):
        super().__init__()
        self.process = process
        self.queue = queue.Queue()
        self.stop = False

    def send_request(self, message):
        self.queue.put(message)

    def send_message(self, message):
        data = json.dumps(message) + "\n"
        self.process.stdin.write(data.encode("utf-8"))  # type: ignore
        self.process.stdin.flush()  # type: ignore

    def run(self):
        try:
            while self.process.poll() is None and not self.stop:
                message = self.queue.get(block=True)
                self.send_message(message)
        except:
            print(traceback.format_exc())


class TabNineReceiver(Thread):
    def __init__(self, process: subprocess.Popen):
        super().__init__()

        self.process = process
        self.queue = queue.Queue()
        self.stop = False

    def get_message(self):
        return self.queue.get(block=True)

    def run(self):
        try:
            while self.process.poll() is None and not self.stop:
                output = self.process.stdout.readline().decode("utf-8")  # type: ignore
                if len(output) > 0:
                    try:
                        data = json_parser.loads(output)
                        self.queue.put(data)
                    except:
                        print(output)
                        print(traceback.format_exc())
        except:
            print(traceback.format_exc())


class Tabnine(object):
    def __init__(self, path: str, manager):
        self.name = "tabnine"
        self.manager = manager
        self.process = None
        self.path = path
        self.receiver = None
        self.sender = None
        self.dispatcher = None
        self.stop = False

    def request(
        self,
        before: str,
        after: str,
        filename: str,
        region_includes_beginning: Union[str, bool],
        region_includes_end: Union[str, bool],
        max_num_results: int,
    ):
        filename = None if not isinstance(filename, str) else filename
        data = {
            "version": TABNINE_PROTOCOL_VERSION,
            "request": {
                "Autocomplete": {
                    "before": before,
                    "after": after,
                    "filename": filename,
                    "region_includes_beginning": bool(region_includes_beginning),
                    "region_includes_end": bool(region_includes_end),
                    "max_num_results": max_num_results,
                }
            },
        }
        if self.process is None:
            self.run()

        self.sender.send_request(data)

    def run(self):
        #  TODO: restart when the process terminates
        if self.process is None:
            self.process = subprocess.Popen(
                [self.path, "--client", "emacs"],
                bufsize=DEFAULT_BUFFER_SIZE,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                # stderr=subprocess.DEVNULL,
                stderr=sys.stderr,
            )

            self.receiver = TabNineReceiver(self.process)
            self.receiver.start()

            self.sender = TabNineSender(self.process)
            self.sender.start()

            self.dispatcher = threading.Thread(target=self.message_dispatcher)
            self.dispatcher.start()

    def message_dispatcher(self):
        try:
            while not self.stop:
                message = self.receiver.get_message()
                if "results" in message and len(message["results"]) > 0:
                    completion_candidates = []
                    for result in message["results"]:
                        candidate = {
                            "new_prefix": result["new_prefix"],
                            "new_suffix": result["new_suffix"],
                            "old_suffix": result["old_suffix"],
                        }
                        if "detail" in result:
                            candidate["detail"] = result["detail"]

                        completion_candidates.append(candidate)
                    self.manager.eval_in_emacs(
                        "tabnine-capf-callback", completion_candidates
                    )
        except:
            print(traceback.format_exc())


class Manager:
    def __init__(self):
        self.server = ThreadingEPCServer(("localhost", 0))
        self.server.allow_reuse_address = True
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.allow_reuse_address = True
        self.client = None
        self.try_completion_timer: Optional[threading.Timer] = None
        self.tabnine = Tabnine(get_tabnine_path(TABNINE_BINARIES_FOLDER), self)
        self.setup()

    def do_completion(self, *data):
        result = self.tabnine.request(*data)
        if result and len(result["results"]) > 0:
            self.eval_in_emacs("tabnine-capf-callback", result)

    def setup(self):
        def complete(
            before: str,
            after: str,
            filename: str,
            region_includes_beginning: Union[str, bool],
            region_includes_end: Union[str, bool],
            max_num_results: int,
        ):
            if (
                self.try_completion_timer is not None
                and self.try_completion_timer.is_alive()
            ):
                self.try_completion_timer.cancel()
                self.try_completion_timer = None

            #  TODO: Do we really wait here?
            self.try_completion_timer = threading.Timer(
                0.1,
                lambda: self.do_completion(
                    before,
                    after,
                    filename,
                    region_includes_beginning,
                    region_includes_end,
                    max_num_results,
                ),
            )
            self.try_completion_timer.start()

        def install_tabnine():
            install_tabnine_at(TABNINE_BINARIES_FOLDER)

        def cleanup():
            if self.client is None:
                self.client.close()
            self.tabnine.stop = True
            self.tabnine.dispatcher.stop = True
            self.tabnine.receiver.stop = True
            if self.tabnine.process is not None:
                self.tabnine.process.terminate()

        self.server.register_function(complete)
        self.server.register_function(install_tabnine)
        self.server.register_function(cleanup)

    def run(self, port):
        try:
            self.client = EPCClient(("localhost", port), log_traceback=True)
        except ConnectionRefusedError:
            print(traceback.format_exc())
        if self.client is not None:
            self.eval_in_emacs(
                "tabnine-capf--first-start", self.server.server_address[1]
            )
            self.server.print_port()
            self.server_thread.start()

    def eval_in_emacs(self, method_name, *args):
        if self.client:
            args = [sexpdata.Symbol(method_name)] + list(map(sexpdata.Quoted, args))
            sexp = sexpdata.dumps(args)
            self.client.call("eval-in-emacs", [sexp])


if __name__ == "__main__":
    Manager().run(int(sys.argv[1]))
