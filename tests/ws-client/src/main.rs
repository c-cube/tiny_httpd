use anyhow::{anyhow, Context, Result};
use clap::Parser;
use futures::{SinkExt, StreamExt};
use std::time::Duration;
use tokio::io::AsyncWriteExt;
use tungstenite::client::IntoClientRequest;
use tungstenite::Message;

#[derive(clap::Parser, Debug)]
struct Connect {
    /// The remote `ws` endpoint
    url: String,
    /// Number of messages to send
    #[arg(short)]
    n: usize,
}

fn make_req(c: Connect) -> Result<tungstenite::http::Request<()>> {
    let req = c.url.clone().into_client_request()?;
    Ok(req)
}

#[derive(serde::Serialize)]
struct MyMsg {
    foo: String,
    args: Vec<String>,
}

async fn tunnel(c: Connect) -> Result<()> {
    let n = c.n;
    let url = c.url.clone();
    let req = make_req(c)?;
    let (mut ws, _res) = tokio_tungstenite::connect_async(req)
        .await
        .with_context(|| anyhow!("connecting to the endpoint {}", url))?;

    let (send_msg, mut recv_msg): (_, tokio::sync::mpsc::Receiver<String>) =
        tokio::sync::mpsc::channel(32);

    let _th = tokio::spawn(async move {
        for i in 0..n {
            let msg = MyMsg {
                foo: format!("foo{i}"),
                args: (0..(30 + (i % 20)))
                    .into_iter()
                    .map(|i| format!("arg{i}"))
                    .collect::<Vec<_>>(),
            };
            let mut s: String = serde_json::to_string(&msg).unwrap();
            s += "\n";
            send_msg.send(s).await.unwrap();
        }

        // sleep to let the other thread have time to finish
        tokio::time::sleep(Duration::from_secs(1)).await;
    });

    let mut stdout = tokio::io::stdout();
    loop {
        tokio::select! {
            msg_to_send = recv_msg.recv() => {
                match msg_to_send {
                    None => break,
                    Some(v) => {
                        let msg = Message::Binary(v.into_bytes());
                        ws.send(msg).await?
                    }
                }

            },
            net_input = ws.next() => {
                let msg =
                match net_input {
                    None => break,
                    Some(m) => m?
                };
                match msg {
                    Message::Text(s) => {
                        stdout.write_all(s.as_bytes()).await?;
                        stdout.flush().await?;
                    },
                    Message::Binary(s) => {
                        stdout.write_all(&s).await?;
                        stdout.flush().await?;
                    },
                    Message::Frame(_) |
                    Message::Ping(_) | Message::Pong(_) => (),
                    Message::Close(_) => break,
                }

            }
        }
    }

    Ok(())
}

fn main() -> Result<()> {
    let c = Connect::try_parse().with_context(|| anyhow!("could not parse CLI arguments"))?;

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_io()
        .enable_time()
        .build()?;
    rt.block_on(async { tunnel(c).await })?;

    Ok(())
}
