import json
import logging
import subprocess
import threading

from epc.server import ThreadingEPCServer


class Tabnine(object):
    def __init__(self, path):
        self.name = "tabnine"
        self._proc = None
        self._response = None
        self.logger = logging.getLogger(self.__class__.__name__)
        self.path = path

    def update_path(self, path):
        self.path = path
        self._restart()

    def request(self, data):
        proc = self._get_running_tabnine()
        if proc is None:
            return
        try:
            if not isinstance(data, str):
                data = json.dumps(data)
            proc.stdin.write((data + "\n").encode("utf8"))
            proc.stdin.flush()
        except BrokenPipeError:
            self._restart()
            return

        output = proc.stdout.readline().decode("utf8")
        try:
            return json.loads(output)
        except json.JSONDecodeError:
            self.logger.debug("Tabnine output is corrupted: " + output)

    def _restart(self):
        if self._proc is not None:
            self._proc.terminate()
            self._proc = None
        if self.path is None:
            self.logger.error("no Tabnine binary found")
            return
        self._proc = subprocess.Popen(
            [
                self.path,
                "--client",
                "emacs",
            ],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
        )

    def _get_running_tabnine(self):
        if self._proc is None:
            self._restart()
        if self._proc is not None and self._proc.poll():
            self.logger.error(
                "Tabnine exited with code {}".format(self._proc.returncode)
            )
            self._restart()
        return self._proc


class Manager:
    def __init__(self):
        self.server = ThreadingEPCServer(("localhost", 0), log_traceback=True)
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.allow_reuse_address = True
        self.try_completion_timer = None

        self.path = None
        self.tabnine = Tabnine(self.path)

        self.server.logger.setLevel(logging.DEBUG)

        self.setup()

    def do_completion(self, data):
        result = self.tabnine.request(data)
        if result:
            self.server.clients[0].call("tabnine-capf-callback", result)

    def setup(self):
        def complete(
            version,
            before,
            after,
            filename,
            region_includes_beginning,
            region_includes_end,
            max_num_results,
        ):
            if (
                self.try_completion_timer is not None
                and self.try_completion_timer.is_alive()
            ):
                self.try_completion_timer.cancel()
                self.try_completion_timer = None

            filename = None if not isinstance(filename, str) else filename
            data = {
                "version": version,
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
            self.try_completion_timer = threading.Timer(
                0.1, lambda: self.do_completion(data)
            )
            self.try_completion_timer.start()

        def set_executable_path(path):
            self.path = path
            self.tabnine.update_path(self.path)

        self.server.register_function(complete)
        self.server.register_function(set_executable_path)

    def main(self):
        self.server_thread.start()
        self.server.print_port()


if __name__ == "__main__":
    m = Manager()
    m.main()
