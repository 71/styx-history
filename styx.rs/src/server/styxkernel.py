## The Styx Kernel for Jupyter.

import requests
from ipykernel.kernelbase import Kernel

class StyxKernel(Kernel):
    implementation = 'styxkernel'
    implementation_version = '0.1'

    language_info = {'name': 'Styx',
                     'codemirror_mode': 'haskell',
                     'mimetype': 'text/x-haskell',
                     'file_extension': '.sx'}

    banner = 'The Styx Kernel.'
    context = None

    address = 'http://127.0.0.1:8008'

    def __init__(self, **kwargs):
        Kernel.__init__(self, **kwargs)

        # Get context
        r = requests.post('{}/ctx/new'.format(self.address))

        self.context = r.text

    def do_execute(self, code, silent, store_history=True, user_expressions=None,
                   allow_stdin=False):

        if not code.strip():
            return {'status': 'ok',
                    'execution_count': self.execution_count,
                    'payload': [],
                    'user_expressions': {}}

        pld = {'context': self.context}
        res = requests.post('{}/ctx/execute'.format(self.address), params=pld, data=code)

        if not silent:
            stream_content = {'name': 'stdout',
                              'text': res.text}

            self.send_response(self.iopub_socket, 'stream', stream_content)

        return {'status': 'ok',
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {}}


if __name__ == '__main__':
    import argparse

    from sys import exit
    from subprocess import Popen
    from ipykernel.kernelapp import IPKernelApp

    # parser = argparse.ArgumentParser(description = 'The Styx Jupyter Kernel.')
    # parser.add_argument('address', help='The address at which the Styx deamon listens.', default='127.0.0.1:8008')
    #
    # args = parser.parse_args()
    # address = None
    #
    # if args.address:
    #     r = requests.get(args.address)
    #
    #     if r.status_code == 200:
    #         address = args.address
    #     else:
    #         print('Could not contact deamon.')
    #         exit(1)
    #
    # if not address:
    #     print('Starting server at localhost:8008...')
    #
    #     Popen(['styx', 'deamon', '127.0.0.1:8008'])
    #     address = 'http://127.0.0.1:8008'

    IPKernelApp.launch_instance(kernel_class=StyxKernel)

