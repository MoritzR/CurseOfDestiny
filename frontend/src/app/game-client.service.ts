import { Injectable, signal } from '@angular/core';
import { ClientMessage, GameSnapshot, Prompt, ServerMessage } from './protocol';

@Injectable({ providedIn: 'root' })
export class GameClientService {
  readonly connectionState = signal<'connecting' | 'open' | 'closed'>('connecting');
  readonly snapshot = signal<GameSnapshot | null>(null);
  readonly prompt = signal<Prompt | null>(null);
  readonly notices = signal<string[]>([]);

  private socket: WebSocket | null = null;

  connect(url = 'ws://127.0.0.1:8080'): void {
    this.socket?.close();
    this.connectionState.set('connecting');

    const socket = new WebSocket(url);
    this.socket = socket;

    socket.onopen = () => {
      this.connectionState.set('open');
    };

    socket.onclose = () => {
      this.connectionState.set('closed');
    };

    socket.onmessage = (event) => {
      const parsed = JSON.parse(event.data) as ServerMessage;
      this.handleMessage(parsed);
    };
  }

  submitCommand(command: string): void {
    this.send({ type: 'SubmitCommand', payload: { command } });
  }

  submitChoice(choiceIndex: number): void {
    this.send({ type: 'SubmitChoice', payload: { choiceIndex } });
  }

  submitNumber(number: number): void {
    this.send({ type: 'SubmitNumber', payload: { number } });
  }

  private send(message: ClientMessage): void {
    if (this.socket?.readyState !== WebSocket.OPEN) {
      return;
    }

    this.socket.send(JSON.stringify(message));
  }

  private handleMessage(message: ServerMessage): void {
    switch (message.type) {
      case 'Connected':
        this.pushNotice(message.payload.message);
        break;
      case 'StateSnapshot':
        this.snapshot.set(message.payload.state);
        break;
      case 'PromptMessage':
        this.prompt.set(message.payload.prompt);
        break;
      case 'NoticeMessage':
        this.pushNotice(message.payload.message);
        break;
      case 'ErrorMessage':
        this.pushNotice(`Error: ${message.payload.message}`);
        break;
    }
  }

  private pushNotice(message: string): void {
    this.notices.update((notices) => [...notices, message]);
  }
}
